{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Device.MQTT
  ( startMQTT
  , newMqttEnv
  , MqttEnv (..)
  , publish
  , request
  ) where
import           Conduit                    (ConduitT, Void, awaitForever,
                                             runConduit, yield, (.|))
import           Control.Concurrent         (forkIO, forkOS, threadDelay)
import           Control.Concurrent.STM     (TChan, TQueue, atomically,
                                             cloneTChan, dupTChan,
                                             newBroadcastTChanIO, newTQueueIO,
                                             readTChan, readTQueue, retry,
                                             writeTChan, writeTQueue)
import           Control.Monad              (forever, void, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Data.ByteString            (ByteString, foldr', packCString,
                                             useAsCString)
import           Data.ByteString.Builder    (toLazyByteString, word8Hex)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import           Data.Cache                 (Cache (..), newCache)
import qualified Data.Cache                 as Cache (deleteSTM, insert',
                                                      insertSTM, lookupSTM,
                                                      purgeExpired)
import           Data.Int                   (Int64)
import           Data.List                  (elemIndex, (!!))
import           Data.String.Utils          (split)
import           Foreign.C.String           (CString, newCString, peekCString)
import           Foreign.C.Types            (CInt (..))
import           Foreign.Ptr                (Ptr)
import           Foreign.Storable           (poke)
import           System.Clock               (Clock (Monotonic), TimeSpec (..),
                                             getTime)
import           System.Entropy             (getEntropy)
import           System.IO.Unsafe           (unsafePerformIO)

#include <hs_mqtt.h>


foreign import ccall safe "hs_mqtt_main"
  c_mqtt_main :: CString -> CString -> CString -> CString -> CString -> IO CInt

foreign export ccall on_message :: CString -> CString -> IO ()

on_message :: CString -> CString -> IO ()
on_message t p = do
  topic <- peekCString t
  payload <- packCString p
  atomically $ writeTChan subscribeChan RpcMsg {..}

data RpcMsg = RpcMsg
  { topic   :: !String
  , payload :: !ByteString
  }

foreign export ccall get_message :: Ptr CString -> Ptr CString -> IO ()

get_message :: Ptr CString -> Ptr CString -> IO ()
get_message ptr0 ptr1 = do
  RpcMsg{..} <- atomically $ readTQueue publishQueue
  t <- newCString topic
  poke ptr0 t
  useAsCString payload $ poke ptr1


publishQueue :: TQueue RpcMsg
publishQueue = unsafePerformIO newTQueueIO

subscribeChan :: TChan RpcMsg
subscribeChan = unsafePerformIO newBroadcastTChanIO

responseCache :: Cache String (Maybe ByteString)
responseCache = unsafePerformIO $ newCache (Just $ TimeSpec 300 0)

-- /:key/:uuid/request/:requestid
mkRequestTopic :: String -> String -> String -> String
mkRequestTopic k uid rid = "/" ++ k ++ "/" ++ uid ++ "/request/" ++ rid

-- /:key/:uuid/response/:responseid
mkResponseTopic :: String -> String -> String -> String
mkResponseTopic k uid rid = "/" ++ k ++ "/" ++ uid ++ "/response/" ++ rid


-- first we need a little helper to use a TChan as source
sourceTChan :: MonadIO m
            => TChan a -> ConduitT () a m ()
sourceTChan chan = forever $ liftIO (atomically (readTChan chan)) >>= yield


-- A Conduit that only yields messages that were published on the argument topic
filterTopic :: Monad m => (RpcMsg -> Bool) -> ConduitT RpcMsg RpcMsg m ()
filterTopic f = awaitForever $ \msg -> when (f msg) $ yield msg


isTopic :: String -> RpcMsg -> Bool
isTopic t RpcMsg{..} = elemIndex t (split "/" topic) == Just 3

getUUID :: String -> String
getUUID s = (split "/" s) !! 2


-- consumers for messages on the topics we are interested in
handleResponseTopic :: ConduitT RpcMsg Void IO ()
handleResponseTopic = awaitForever $ \RpcMsg{..} ->
  lift $ do
    now <- getTime Monotonic
    let t = case defaultExpiration responseCache of
              Nothing -> Nothing
              Just t' -> Just $ now + t'
    atomically $ do
      r <- Cache.lookupSTM True topic responseCache now
      case r of
        Nothing -> pure ()
        Just _  -> Cache.insertSTM topic (Just payload) responseCache t

handler :: (String -> ByteString -> IO ()) -> ConduitT RpcMsg Void IO ()
handler f = awaitForever $ \RpcMsg{..} ->
  lift $ f (getUUID topic) payload

data MqttEnv = MqttEnv
  { saveAttributes :: String -> ByteString -> IO ()  -- saveAttributes uuid payload
  , saveTelemetry  :: String -> ByteString -> IO () -- saveTelemetry uuid payload
  , mKey           :: String -- the service key
  , mUsername      :: String
  , mPassword      :: String
  , mHost          :: String
  , mPort          :: String
  }


newMqttEnv :: String -> IO MqttEnv
newMqttEnv k = do
  return MqttEnv
    { saveAttributes = \_ _ -> pure ()
    , saveTelemetry  = \_ _ -> pure ()
    , mKey = k
    , mUsername = ""
    , mPassword = ""
    , mHost = "localhost"
    , mPort = "1883"
    }


publish :: String -> ByteString -> IO ()
publish topic payload = atomically $ writeTQueue publishQueue RpcMsg {..}

genHex :: Int -> IO String
genHex n = fix . prettyPrint <$> getEntropy n
  where prettyPrint :: ByteString -> String
        prettyPrint =  LB.unpack . toLazyByteString . mconcat . foldr'
                ( \ byte acc -> word8Hex byte:acc ) []

        fix :: String -> String
        fix v | length v `mod` 2 == 0 = v
              | otherwise = 'a' : v

-- request key uuid data timeout
request :: String -> String -> ByteString -> Int64 -> IO (Maybe ByteString)
request key uuid p t = do
  reqid <- genHex 4

  let k = mkResponseTopic key uuid reqid

  Cache.insert' responseCache (Just $ TimeSpec t 0) k Nothing

  publish (mkRequestTopic key uuid reqid) p

  now <- getTime Monotonic

  atomically $ do
    r <- Cache.lookupSTM True k responseCache now
    case r of
      Nothing -> pure Nothing
      Just Nothing -> retry
      Just v -> do
        Cache.deleteSTM k responseCache
        pure v

startMQTT :: MqttEnv -> IO ()
startMQTT env = do
  host <- newCString $ mHost env
  port <- newCString $ mPort env
  clientId <- newCString $ mKey env
  username <- newCString $ mUsername env
  password <- newCString $ mPassword env

  void $ forkOS $ void $ c_mqtt_main host port clientId username password

  pubChan0 <- atomically $ dupTChan subscribeChan
  pubChan1 <- atomically $ cloneTChan pubChan0
  pubChan2 <- atomically $ cloneTChan pubChan0
  void
    $ forkIO
    $ runConduit
    $ sourceTChan pubChan0
    .| filterTopic (isTopic "response")
    .| handleResponseTopic
  void
    $ forkIO
    $ runConduit
    $ sourceTChan pubChan1
    .| filterTopic (isTopic "attributes")
    .| handler (saveAttributes env)
  void
    $ forkIO
    $ runConduit
    $ sourceTChan pubChan2
    .| filterTopic (isTopic "telemetry")
    .| handler (saveTelemetry env)

  forever $ do
    Cache.purgeExpired responseCache
    threadDelay $ 1000000 * 1
