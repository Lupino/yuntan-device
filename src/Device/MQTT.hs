{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Device.MQTT
  ( startMQTT
  , MqttEnv (..)
  , request
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
import           Data.Text              (Text, append, pack, splitOn)
import           Network.MQTT.Client
import           Network.URI            (URI, uriFragment)
import           System.Clock           (Clock (Monotonic), TimeSpec (..),
                                         getTime)
import           System.Entropy         (getEntropy)

type ResponseCache = Cache Text (Maybe ByteString)
type RequestCache  = Cache Text (Maybe ByteString)

data MqttEnv = MqttEnv
    { mKey      :: String -- the service key
    , mClient   :: TVar (Maybe MQTTClient)
    , mResCache :: ResponseCache
    , mReqCache :: RequestCache
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
genHex n = concatMap w . B.unpack <$> getEntropy n
  where w ch = let s = "0123456789ABCDEF"
                   x = fromEnum ch
               in [s !! div x 16,s !! mod x 16]


-- request env uuid data timeout
request :: MqttEnv -> Text -> ByteString -> Int64 -> IO (Maybe ByteString)
request MqttEnv {..} uuid p t = do
  reqid <- pack <$> genHex 4

  let k = responseKey uuid reqid

  Cache.insert' mResCache (Just $ TimeSpec t 0) k Nothing

  client <- readTVarIO mClient
  case client of
    Nothing -> return Nothing
    Just c -> do
      publish c (requestTopic mKey uuid reqid) p False

      now <- getTime Monotonic

      atomically $ do
        r <- Cache.lookupSTM True k mResCache now
        case r of
          Nothing -> pure Nothing
          Just Nothing -> retry
          Just v -> do
            Cache.deleteSTM k mResCache
            pure v

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

messageCallback :: (Text -> ByteString -> IO ()) -> ResponseCache -> MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
messageCallback saveAttributes resCache _ topic payload _ =
  case splitOn "/" topic of
    (_:_:uuid:"response":reqid:_) -> do
      let k = responseKey uuid reqid
      now <- getTime Monotonic
      let t = case defaultExpiration resCache of
                Nothing -> Nothing
                Just t' -> Just $ now + t'
      atomically $ do
        r <- Cache.lookupSTM True k resCache now
        case r of
          Nothing -> pure ()
          Just _  -> Cache.insertSTM k (Just payload) resCache t
    (_:_:uuid:"attributes":_) -> saveAttributes uuid payload
    _ -> pure ()

startMQTT :: String -> URI -> (Text -> ByteString -> IO ())-> IO MqttEnv
startMQTT key mqttURI saveAttributes = do
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
      client <- connectURI conf mqttURI { uriFragment = clientId }
      atomically $ writeTVar mc $ Just client
      print =<< subscribe client [(responseTopic key, subOptions), (attrTopic key, subOptions)] []
      print =<< waitForClient client   -- wait for the the client to disconnect
      atomically $ writeTVar mc Nothing

    case r of
      Left (e::SomeException) -> print e
      Right _                 -> pure ()

    threadDelay 1000000

  void $ forkIO $ forever $ do
    Cache.purgeExpired resCache
    Cache.purgeExpired reqCache
    threadDelay 1000000

  pure MqttEnv
    { mKey = key
    , mClient = mc
    , mResCache = resCache
    , mReqCache = reqCache
    }
