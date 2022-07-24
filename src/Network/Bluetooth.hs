{-# LANGUAGE OverloadedStrings #-}

module Network.Bluetooth
    ( DBusClient
    , Device (..)
    , BluetoothError (..)
    , newClient
    , devices
    , connect
    , disconnect
    , pairing
    , cancelPairing
    , remove
    , trust
    , unTrust
    , block
    , unBlock
    , startDiscovery
    , stopDiscovery
    )
where

import           Control.Exception  (Exception, Handler (Handler), catches,
                                     throwIO)
import           Control.Monad      (void)
import           DBus               (BusName, IsVariant, MemberName,
                                     MethodReturn, ObjectPath, Variant,
                                     dictionaryItems, fromVariant, methodCall,
                                     methodCallBody, methodCallDestination,
                                     methodReturnBody, toVariant)
import           DBus.Client        (Client, ClientError (..), call_,
                                     connectSystem)
import qualified DBus.Introspection as I
import qualified Data.Map           as M
import           Data.Maybe         (listToMaybe, mapMaybe)
import           Data.Typeable      (Typeable)

newtype DBusClient = DBusClient Client

data Device = Device
    { devObjectPath       :: ObjectPath
    , devAdapter          :: Maybe ObjectPath
    , devAddress          :: Maybe String
    , devAddressType      :: Maybe String
    , devAlias            :: Maybe String
    , devBlocked          :: Maybe Bool
    , devConnected        :: Maybe Bool
    , devIcon             :: Maybe String
    , devLegacyPairing    :: Maybe Bool
    , devModalias         :: Maybe String
    , devName             :: Maybe String
    , devPaired           :: Maybe Bool
    , devServicesResolved :: Maybe Bool
    , devTrusted          :: Maybe Bool
    , devWakeAllowed      :: Maybe Bool
    , devUUIDs            :: Maybe [String]
    }
    deriving (Eq, Show)

newtype BluetoothError = BluetoothError
    { bluetoothErrorMessage :: String
    }
    deriving (Eq, Show, Typeable)

instance Exception BluetoothError

bluetoothError :: String -> BluetoothError
bluetoothError = BluetoothError

data InvalidDataError = InvalidDataError
    { invalidDataErrorMessage :: String
    , invalidDataBody         :: String
    }
    deriving (Eq, Show, Typeable)

instance Exception InvalidDataError

invalidDataError :: Show a => String -> a -> InvalidDataError
invalidDataError msg body = InvalidDataError msg (show body)

exceptionHandlers :: [Handler a]
exceptionHandlers =
    [ Handler clientErrorHandler
    , Handler replyDataErrorHandler
    ]
  where
    clientErrorHandler :: ClientError -> IO a
    clientErrorHandler ex =
        throwIO . bluetoothError $ "ClientError: " ++ clientErrorMessage ex
    replyDataErrorHandler :: InvalidDataError -> IO a
    replyDataErrorHandler ex =
        throwIO . bluetoothError $ "InvalidDataError: " ++ invalidDataErrorMessage ex

newClient :: IO DBusClient
newClient = DBusClient <$> connectSystem

devices :: DBusClient -> IO [Device]
devices (DBusClient client) = do
    paths <- getDevicePaths client
    mapM (getDeviceInfo client) paths
    `catches` exceptionHandlers

connect :: DBusClient -> Device -> IO ()
connect = deviceMethodCall "Connect"

disconnect :: DBusClient -> Device -> IO ()
disconnect = deviceMethodCall "Disconnect"

pairing :: DBusClient -> Device -> IO ()
pairing = deviceMethodCall "Pair"

cancelPairing :: DBusClient -> Device -> IO ()
cancelPairing = deviceMethodCall "CancelPairing"

remove :: DBusClient -> Device -> IO ()
remove = adapterMethodCall "RemoveDevice"

trust :: DBusClient -> Device -> IO ()
trust = devicePropertiesSet "Trusted" True

unTrust :: DBusClient -> Device -> IO ()
unTrust = devicePropertiesSet "Trusted" False

block :: DBusClient -> Device -> IO ()
block = devicePropertiesSet "Blocked" True

unBlock :: DBusClient -> Device -> IO ()
unBlock = devicePropertiesSet "Blocked" False

startDiscovery :: DBusClient -> IO ()
startDiscovery (DBusClient client) = do
    apaths <- getAdapterPaths client
    mapM_ (\path -> do
        void $ call_ client (methodCall path "org.bluez.Adapter1" "StartDiscovery")
            { methodCallDestination = Just "org.bluez"
            }
        `catches` exceptionHandlers) apaths

stopDiscovery :: DBusClient -> IO ()
stopDiscovery (DBusClient client) = do
    apaths <- getAdapterPaths client
    mapM_ (\path -> do
        void $ call_ client (methodCall path "org.bluez.Adapter1" "StopDiscovery")
            { methodCallDestination = Just "org.bluez"
            }
        `catches` exceptionHandlers) apaths

deviceMethodCall :: MemberName -> DBusClient -> Device -> IO ()
deviceMethodCall memberName (DBusClient client) Device { devObjectPath = path } = do
    void $ call_ client (methodCall path "org.bluez.Device1" memberName)
        { methodCallDestination = Just "org.bluez"
        }
    `catches` exceptionHandlers

adapterMethodCall :: MemberName -> DBusClient -> Device -> IO ()
adapterMethodCall memberName (DBusClient client)
        Device { devObjectPath = dpath, devAdapter = madapter } = do
    apath <- maybe
        (throwIO $ invalidDataError "adapter not found on device" dpath) return madapter
    void $ call_ client (methodCall apath "org.bluez.Adapter1" memberName)
        { methodCallDestination = Just "org.bluez"
        , methodCallBody = map toVariant [ dpath ]
        }
    `catches` exceptionHandlers

devicePropertiesSet :: (IsVariant a) => String -> a -> DBusClient -> Device -> IO ()
devicePropertiesSet name value (DBusClient client)
        Device { devObjectPath = path } =
    dbusSet client path "org.bluez.Device1" name value
    `catches` exceptionHandlers

getAdapterPaths :: Client -> IO [ObjectPath]
getAdapterPaths client = do
    bluez <- introspectBluez client "/org/bluez"
    adapters <- mapM (introspectBluez client . I.objectPath)
             $ I.objectChildren bluez
    return $ map I.objectPath adapters

getDevicePaths :: Client -> IO [ObjectPath]
getDevicePaths client = do
    paths <- getAdapterPaths client
    concatMap (map I.objectPath . I.objectChildren)
      <$> mapM (introspectBluez client) paths

getDeviceInfo :: Client -> ObjectPath -> IO Device
getDeviceInfo client path = do
    toDevice path . M.fromList . mapMaybe convProp . dictionaryItems
        <$> dbusGetAll client path "org.bluez.Device1"
  where
    convProp :: (Variant, Variant) -> Maybe (String, Variant)
    convProp (k,v) = do
        k' <- fromVariant k
        v' <- fromVariant v
        return (k',v')

toDevice :: ObjectPath -> M.Map String Variant -> Device
toDevice path m =
    Device
        { devObjectPath = path
        , devAdapter = getV "Adapter"
        , devAddress = getV "Address"
        , devAddressType = getV "AddressType"
        , devAlias = getV "Alias"
        , devBlocked = getV "Blocked"
        , devConnected = getV "Connected"
        , devIcon = getV "Icon"
        , devLegacyPairing = getV "LegacyPairing"
        , devModalias = getV "Modalias"
        , devName = getV "Name"
        , devPaired = getV "Paired"
        , devServicesResolved = getV "ServicesResolved"
        , devTrusted = getV "Trusted"
        , devWakeAllowed = getV "WakeAllowed"
        , devUUIDs = getV "UUIDs"
        }
  where
    getV s = fromVariant =<< m M.!? s

dbusGetAll :: (IsVariant a) => Client -> ObjectPath -> String -> IO a
dbusGetAll client path target = do
    reply <- call_ client (methodCall path "org.freedesktop.DBus.Properties" "GetAll")
        { methodCallDestination = Just "org.bluez"
        , methodCallBody = map toVariant [ target ]
        }
    either throwIO return $ convertMethodReturn reply

-- dbusGet :: (IsVariant a) => Client -> ObjectPath -> String -> String -> IO a
-- dbusGet client path target name = do
--     reply <- call_ client (methodCall path "org.freedesktop.DBus.Properties" "Get")
--         { methodCallDestination = Just "org.bluez"
--         , methodCallBody = map toVariant [ target, name ]
--         }
--     either throwIO return $ convertMethodReturn reply

dbusSet :: (IsVariant a) => Client -> ObjectPath -> String -> String -> a -> IO ()
dbusSet client path target name value = do
    void $ call_ client (methodCall path "org.freedesktop.DBus.Properties" "Set")
        { methodCallDestination = Just "org.bluez"
        , methodCallBody = [ toVariant target, toVariant name, toVariant (toVariant value) ]
        }

dbusIntrospect :: Client -> BusName -> ObjectPath -> IO I.Object
dbusIntrospect client service path = do
    reply <- call_ client (methodCall path "org.freedesktop.DBus.Introspectable" "Introspect")
        { methodCallDestination = Just service
        }
    xml <- either throwIO return $ convertMethodReturn reply
    case I.parseXML path xml of
        Just info -> return info
        Nothing   -> throwIO $ invalidDataError "invalid introspection XML" xml

introspectBluez :: Client -> ObjectPath -> IO I.Object
introspectBluez client = dbusIntrospect client "org.bluez"

convertMethodReturn :: (IsVariant a) => MethodReturn -> Either InvalidDataError a
convertMethodReturn reply =
    let mb = fromVariant =<< listToMaybe (methodReturnBody reply)
     in case mb of
         Just v  -> Right v
         Nothing -> Left $ invalidDataError "invalid method return body" (methodReturnBody reply)
