{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Device.MQTT
  ( startMQTT
  , MqttEnv (..)
  , request
  ) where
import           Conduit                   (ConduitT, Void, awaitForever,
                                            runConduit, yield, (.|))
import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.STM    (TChan, TVar, atomically, cloneTChan,
                                            newTChanIO, newTVarIO, readTChan,
                                            readTVarIO, retry, writeTVar)
import           Control.Exception         (SomeException, try)
import           Control.Monad             (forever, void, when)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8     as B (unpack)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Cache                (Cache (..), newCache)
import qualified Data.Cache                as Cache (deleteSTM, insert',
                                                     insertSTM, lookupSTM,
                                                     purgeExpired)
import           Data.Hex                  (hex)
import           Data.Int                  (Int64)
import           Data.String               (fromString)
import           Data.Text                 (Text, append, pack, splitOn, unpack)
import           Device.Config             (MqttConfig (..))
import           Network.MQTT.Client
import           System.Clock              (Clock (Monotonic), TimeSpec (..),
                                            getTime)
import           System.Entropy            (getEntropy)

type ResponseCache = Cache Text (Maybe ByteString)

data MqttEnv = MqttEnv
  { mKey    :: String -- the service key
  , mClient :: TVar MQTTClient
  , mCache  :: ResponseCache
  }

-- /:key/:uuid/request/:requestid
requestTopic :: String -> Text -> Text -> Topic
requestTopic k uid rid = "/" <> pack k <> "/" <> uid <> "/request/" <> rid

responseKey :: Text -> Text -> Text
responseKey = append

-- /:key/:uuid/response/:responseid
responseTopic :: String -> Topic
responseTopic k = "/" <> pack k <> "/+/response/+"

-- /:key/:uuid/attributes
attrTopic :: String -> Topic
attrTopic k = "/" <> pack k <> "/+/attributes"

genHex :: Int -> IO String
genHex n = hex . B.unpack <$> getEntropy n

-- request env uuid data timeout
request :: MqttEnv -> Text -> ByteString -> Int64 -> IO (Maybe ByteString)
request MqttEnv {..} uuid p t = do
  reqid <- pack <$> genHex 4

  let k = responseKey uuid reqid

  Cache.insert' mCache (Just $ TimeSpec t 0) k Nothing

  client <- readTVarIO mClient

  publish client (requestTopic mKey uuid reqid) p False

  now <- getTime Monotonic

  atomically $ do
    r <- Cache.lookupSTM True k mCache now
    case r of
      Nothing -> pure Nothing
      Just Nothing -> retry
      Just v -> do
        Cache.deleteSTM k mCache
        pure v

messageCallback :: (Text -> ByteString -> IO ()) -> ResponseCache -> MQTTClient -> Topic -> ByteString -> IO ()
messageCallback saveAttributes cache _ topic payload =
  case splitOn "/" topic of
    (_:_:uuid:"response":reqid:_) -> do
      let k = responseKey uuid reqid
      now <- getTime Monotonic
      let t = case defaultExpiration cache of
                Nothing -> Nothing
                Just t' -> Just $ now + t'
      atomically $ do
        r <- Cache.lookupSTM True k cache now
        case r of
          Nothing -> pure ()
          Just _  -> Cache.insertSTM k (Just payload) cache t
    (_:_:uuid:"attributes":_) -> saveAttributes uuid payload
    _ -> pure ()

startMQTT :: String -> MqttConfig -> (Text -> ByteString -> IO ())-> IO MqttEnv
startMQTT key MqttConfig{..} saveAttributes = do
  cache <- newCache (Just $ TimeSpec 300 0)
  mc <- newTVarIO $ error "not initial"

  clientId <- genHex 20

  let conf = mqttConfig
        { _hostname = mqttHost
        , _port     = mqttPort
        , _connID   = clientId
        , _username = Just mqttUsername
        , _password = Just mqttPassword
        , _msgCB = Just (messageCallback saveAttributes cache)
        }

  void $ forkIO $ forever $ do
    r <- try $ do
      client <- runClient conf
      atomically $ writeTVar mc client
      print =<< subscribe client [(responseTopic key, QoS0), (attrTopic key, QoS0)]
      print =<< waitForClient client   -- wait for the the client to disconnect

    case r of
      Left (e::SomeException) -> print e
      Right _                 -> pure ()

    threadDelay 1000000

  void $ forkIO $ forever $ do
    Cache.purgeExpired cache
    threadDelay 1000000

  pure MqttEnv
    { mKey = key
    , mClient = mc
    , mCache = cache
    }
