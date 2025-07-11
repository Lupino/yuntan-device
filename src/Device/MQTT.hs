{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Device.MQTT
  ( startMQTT
  , MqttEnv (..)
  , request
  , sendDrop
  , cacheAble
  ) where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM (TVar, atomically, newTVarIO,
                                         readTVarIO, retry, writeTVar)
import           Control.Exception      (SomeException, try)
import           Control.Monad          (forever, void)
import qualified Data.ByteString.Char8  as B (unpack)
import           Data.ByteString.Lazy   (ByteString)
import           Data.Cache             (Cache (..), newCache)
import qualified Data.Cache             as Cache (deleteSTM, insert', insertSTM,
                                                  lookupSTM, purgeExpired)
import           Data.Int               (Int64)
import           Data.Maybe             (catMaybes, fromMaybe, listToMaybe)
import           Data.Text              (Text, append, pack, splitOn)
import           Device.Types           (Key (..), UUID (..))
import           Network.MQTT.Client
import           Network.MQTT.Topic     (Filter, mkFilter, mkTopic, unTopic)
import           Network.URI            (URI, uriFragment)
import           System.Clock           (Clock (Monotonic), TimeSpec (..),
                                         getTime)
import           System.Entropy         (getEntropy)
import           System.Log.Logger      (errorM)

type ResponseCache = Cache Text (Maybe ByteString)
type RequestCache  = Cache Text (Maybe ByteString)

data MqttEnv = MqttEnv
  { mKey       :: Key -- the service key
  , mClient    :: TVar (Maybe MQTTClient)
  , mResCache  :: ResponseCache
  , mReqCache  :: RequestCache
  , mAllowKeys :: [Key]
  }

-- /:key/:uuid/request/:requestid
requestTopic :: Key -> UUID -> Text -> Maybe Topic
requestTopic k uid rid = mkTopic $ "/" <> unKey k <> "/" <> unUUID uid <> "/request/" <> rid

responseKey :: Text -> Text -> Text
responseKey = append

-- /:key/:uuid/response/:responseid
responseFilter :: Key -> Maybe Filter
responseFilter k = mkFilter $ "/" <> unKey k <> "/+/response/+"

-- /:key/:uuid/telemetry
telemetryFilter :: Key -> Maybe Filter
telemetryFilter k = mkFilter $ "/" <> unKey k <> "/+/telemetry"

-- /:key/:uuid/ping
pingFilter :: Key -> Maybe Filter
pingFilter k = mkFilter $ "/" <> unKey k <> "/+/ping"

-- /:key/:uuid/attributes
attrFilter :: Key -> Maybe Filter
attrFilter k = mkFilter $ "/" <> unKey k <> "/+/attributes"

-- /:key/:uuid/drop
dropTopic :: Key -> UUID -> Maybe Topic
dropTopic k uuid  = mkTopic $ "/" <> unKey k <> "/" <> unUUID uuid <> "/drop"

genHex :: Int -> IO String
genHex n = concatMap w . B.unpack <$> getEntropy n
  where w ch = let s = "0123456789ABCDEF"
                   x = fromEnum ch
               in [s !! div x 16,s !! mod x 16]


-- request env uuid data timeout
request :: MqttEnv -> UUID -> ByteString -> Int64 -> IO (Maybe ByteString)
request MqttEnv {..} uuid p t = do
  reqid <- pack <$> genHex 4

  let k = responseKey (unUUID uuid) reqid

  Cache.insert' mResCache (Just $ TimeSpec t 0) k Nothing

  client <- readTVarIO mClient
  case client of
    Nothing -> return Nothing
    Just c -> do
      case requestTopic mKey uuid reqid of
        Nothing -> return Nothing
        Just topic -> do
          publish c topic p False

          now <- getTime Monotonic

          atomically $ do
            r <- Cache.lookupSTM True k mResCache now
            case r of
              Nothing -> pure Nothing
              Just Nothing -> retry
              Just v -> do
                Cache.deleteSTM k mResCache
                pure v


-- sendDrop env uuid
sendDrop :: MqttEnv -> UUID -> IO ()
sendDrop MqttEnv {..} uuid = do
  client <- readTVarIO mClient
  case client of
    Nothing -> return ()
    Just c  ->
      case dropTopic mKey uuid of
        Nothing    -> return ()
        Just topic -> publish c topic "" False

cacheAble :: MqttEnv -> Text -> Int64 -> IO (Maybe ByteString) -> IO (Maybe ByteString)
cacheAble MqttEnv {..} h t io = do
  now <- getTime Monotonic
  r <- atomically $ do
    r <- Cache.lookupSTM True h mReqCache now
    case r of
      Just Nothing  -> retry
      Just (Just v) -> pure (Just v)
      Nothing       -> do
        Cache.insertSTM h Nothing mReqCache (Just $ now + TimeSpec t 0)
        pure Nothing

  case r of
    Just v -> return $ Just v
    Nothing -> do
      ro <- io
      case ro of
        Nothing -> return Nothing
        Just vo -> do
          Cache.insert' mReqCache (Just $ TimeSpec t 0) h (Just vo)
          return $ Just vo

messageCallback
  :: (Key -> UUID -> ByteString -> Bool -> IO ())
  -> ResponseCache -> MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
messageCallback saveAttributes resCache _ topic payload _ =
  case splitOn "/" (unTopic topic) of
    (_:key:uuid:"response":reqid:_) -> do
      let k = responseKey uuid reqid
      now <- getTime Monotonic
      let t = case defaultExpiration resCache of
                Nothing -> Nothing
                Just t' -> Just $ now + t'
      saveAttributes (Key key) (UUID uuid) payload False
      atomically $ do
        r <- Cache.lookupSTM True k resCache now
        case r of
          Nothing -> pure ()
          Just _  -> Cache.insertSTM k (Just payload) resCache t
    (_:key:uuid:"attributes":_) -> saveAttributes (Key key) (UUID uuid) payload True
    (_:key:uuid:"telemetry":_)  -> saveAttributes (Key key) (UUID uuid) online True
    (_:key:uuid:"ping":_)       -> saveAttributes (Key key) (UUID uuid) online True
    _ -> pure ()

  where online = "{\"state\": \"online\"}"


mkSubscribe :: (Key -> Maybe Filter) -> Key -> Maybe (Filter, SubOptions)
mkSubscribe f k = case f k of
                    Nothing -> Nothing
                    Just t  -> Just (t, subOptions)


startMQTT :: [Key] -> URI -> (Key -> UUID -> ByteString -> Bool -> IO ())-> IO MqttEnv
startMQTT keys mqttURI saveAttributes = do
  resCache <- newCache (Just $ TimeSpec 300 0)
  reqCache <- newCache (Just $ TimeSpec 10 0)

  mc <- newTVarIO Nothing

  clientId <- genHex 20

  let conf = mqttConfig
        { _msgCB = SimpleCallback (messageCallback saveAttributes resCache)
        , _protocol = Protocol311
        }

  void $ forkIO $ forever $ do
    r <- try $ do
      client <- connectURI conf mqttURI { uriFragment = '#':clientId }
      atomically $ writeTVar mc $ Just client
      subscribed <- subscribe client (concatMap catMaybes
        [ map (mkSubscribe responseFilter) keys
        , map (mkSubscribe attrFilter) keys
        , map (mkSubscribe telemetryFilter) keys
        , map (mkSubscribe pingFilter) keys
        ]) []
      errorM "Device.MQTT" $ "Subscribed: " ++ show subscribed
      waited <- waitForClient client   -- wait for the the client to disconnect
      errorM "Device.MQTT" $ "Waited: " ++ show waited
      atomically $ writeTVar mc Nothing

    case r of
      Left (e::SomeException) -> errorM "Device.MQTT" $ "MqttClient Error: " ++ show e
      Right _                 -> pure ()

    threadDelay 1000000

  void $ forkIO $ forever $ do
    Cache.purgeExpired resCache
    Cache.purgeExpired reqCache
    threadDelay 1000000

  pure MqttEnv
    { mKey = fromMaybe "" $ listToMaybe keys
    , mClient = mc
    , mResCache = resCache
    , mReqCache = reqCache
    , mAllowKeys = keys
    }
