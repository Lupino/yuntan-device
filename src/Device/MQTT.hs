{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.MQTT
  ( startMQTT
  , MqttEnv (..)
  , publish
  , request
  ) where
import           Conduit                    (ConduitT, Void, awaitForever,
                                             runConduit, yield, (.|))
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.STM     (TChan, atomically, cloneTChan,
                                             newTChanIO, readTChan, retry)
import           Control.Exception          (SomeException, try)
import           Control.Monad              (forever, void, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Data.ByteString            (ByteString, foldr')
import           Data.ByteString.Builder    (toLazyByteString, word8Hex)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import           Data.Cache                 (Cache (..), newCache)
import qualified Data.Cache                 as Cache (deleteSTM, insert',
                                                      insertSTM, lookupSTM,
                                                      purgeExpired)
import           Data.Int                   (Int64)
import           Data.String                (fromString)
import           Data.Text                  (Text, append, pack, unpack)
import           Device.Config              (MqttConfig (..))
import           Network.MQTT
import           System.Clock               (Clock (Monotonic), TimeSpec (..),
                                             getTime)
import           System.Entropy             (getEntropy)

type ResponseCache = Cache Text (Maybe ByteString)

data MqttEnv = MqttEnv
  { mKey    :: String -- the service key
  , mConfig :: Config
  , mCache  :: ResponseCache
  }

-- /:key/:uuid/request/:requestid
requestTopic :: String -> String -> String -> Topic
requestTopic k uid rid = fromString $ "/" ++ k ++ "/" ++ uid ++ "/request/" ++ rid

-- /:key/:uuid/response/:responseid
mkResponseTopic :: String -> String -> String -> String
mkResponseTopic k uid rid = "/" ++ k ++ "/" ++ uid ++ "/response/" ++ rid

responseKey :: Text -> Text -> Text
responseKey = append

-- /:key/:uuid/response/:responseid
responseTopic :: String -> Topic
responseTopic k = fromString $ "/" ++ k ++ "/+/response/+"

-- /:key/:uuid/attributes
attrTopic :: String -> Topic
attrTopic k = fromString $ "/" ++ k ++ "/+/attributes"


-- first we need a little helper to use a TChan as source
sourceTChan :: MonadIO m
            => TChan a -> ConduitT () a m ()
sourceTChan chan = forever $ liftIO (atomically (readTChan chan)) >>= yield

-- A Conduit that only yields messages that were published on the argument topic
filterTopic :: Monad m
            => Topic
            -> ConduitT (Message PUBLISH) (Message PUBLISH) m ()
filterTopic t = awaitForever $ \msg ->
    when (t `matches` topic (body msg)) $
      yield msg

-- consumers for messages on the topics we are interested in
handleResponse :: ResponseCache -> ConduitT (Message PUBLISH) Void IO ()
handleResponse cache = awaitForever $ \msg ->
  case getLevels $ topic $ body msg of
    (_:_:uuid:"response":reqid:_) -> lift $ do
      let k = responseKey uuid reqid
      now <- getTime Monotonic
      let t = case defaultExpiration cache of
                Nothing -> Nothing
                Just t' -> Just $ now + t'
      atomically $ do
        r <- Cache.lookupSTM True k cache now
        case r of
          Nothing -> pure ()
          Just _  -> Cache.insertSTM k (Just (payload $ body msg)) cache t
    _ -> pure ()

handler :: (String -> ByteString -> IO ()) -> ConduitT (Message PUBLISH) Void IO ()
handler f = awaitForever $ \msg ->
  case getLevels (topic $ body msg) of
    (_:_:uuid:_) -> lift $ f (unpack uuid) (payload $ body msg)
    t            -> lift $ print t


genHex :: Int -> IO String
genHex n = fix . prettyPrint <$> getEntropy n
  where prettyPrint :: ByteString -> String
        prettyPrint =  LB.unpack . toLazyByteString . mconcat . foldr'
                ( \ byte acc -> word8Hex byte:acc ) []

        fix :: String -> String
        fix v | length v `mod` 2 == 0 = v
              | otherwise = 'a' : v

-- request env uuid data timeout
request :: MqttEnv -> String -> ByteString -> Int64 -> IO (Maybe ByteString)
request MqttEnv {..} uuid p t = do
  reqid <- genHex 4

  let k = responseKey (pack uuid) (pack reqid)

  Cache.insert' mCache (Just $ TimeSpec t 0) k Nothing

  publish mConfig Handshake False (requestTopic mKey uuid reqid) p

  now <- getTime Monotonic

  atomically $ do
    r <- Cache.lookupSTM True k mCache now
    case r of
      Nothing -> pure Nothing
      Just Nothing -> retry
      Just v -> do
        Cache.deleteSTM k mCache
        pure v

startMQTT :: String -> MqttConfig -> (String -> ByteString -> IO ())-> IO MqttEnv
startMQTT key MqttConfig{..} saveAttributes = do

  cache <- newCache (Just $ TimeSpec 300 0)

  cmds <- mkCommands
  -- create one channel per conduit, each one receiving all the messages
  pubChan0 <- newTChanIO
  pubChan1 <- atomically $ cloneTChan pubChan0
  let conf = (defaultConfig cmds pubChan0)
              { cUsername  = Just mqttUsername
              , cPassword  = Just mqttPassword
              , cClientID  = pack key
              , cHost      = mqttHost
              , cPort      = fromIntegral mqttPort
              , cKeepAlive = Just 4000
              , cVer       = "3.1.1"
              }

  void
    $ forkIO
    $ runConduit
    $ sourceTChan pubChan0
    .| filterTopic (responseTopic key)
    .| handleResponse cache

  void
    $ forkIO
    $ runConduit
    $ sourceTChan pubChan1
    .| filterTopic (attrTopic key)
    .| handler saveAttributes

  -- this will throw IOExceptions
  void $ forkIO $ forever $ do
    void $ forkIO $ void $ subscribe conf [(responseTopic key, Handshake), (attrTopic key, Handshake)]
    terminated <- try $ run conf :: IO (Either SomeException Terminated)
    print terminated
    -- retry 1 seconds
    threadDelay 1000000

  void $ forkIO $ forever $ do
    Cache.purgeExpired cache
    threadDelay 1000000

  pure MqttEnv
    { mKey = key
    , mConfig = conf
    , mCache = cache
    }
