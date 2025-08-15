{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Device.API
  ( getDevice
  , updateDeviceMeta
  , updateDevice
  , removeDevice
  , updateDeviceMetaByUUID

  , getDevId
  , randomAddr

  , setPingAt

  , saveMetric
  , removeMetric
  , dropMetric

  , getEpochTimeInt

  , module X
  ) where


import           Control.Monad          (forM, unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value (..), decode, encode, object,
                                         (.=))
import           Data.Aeson.Helper      (union)
import qualified Data.Aeson.Key         as Key (Key, fromString, toString)
import qualified Data.Aeson.KeyMap      as KeyMap (filterWithKey, lookup,
                                                   member, toList)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Lazy   as LB (ByteString, fromStrict, toStrict)
import           Data.Foldable          (for_)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe)
import           Data.Scientific        (toRealFloat)
import           Data.String            (fromString)
import           Data.Text              (Text, replace, toLower)
import qualified Data.Text              as T (drop, take, unpack)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.UnixTime
import           Database.PSQL.Types    (HasOtherEnv, HasPSQL)
import           Device.Config          (Cache, redisEnv)
import           Device.RawAPI          as X (countDevAddrByGw, countDevice,
                                              countDeviceByKey, countIndex,
                                              countMetric, createDevice,
                                              createTable, getDevIdByCol,
                                              getDevIdList, getDevIdListByGw,
                                              getDevIdListByKey, getDevKeyById,
                                              getDevKeyId, getIndexDevIdList,
                                              getIndexNameId, getIndexNameId_,
                                              getMetric, getMetricIdList,
                                              removeIndex, removeIndexName,
                                              saveIndex)
import qualified Device.RawAPI          as RawAPI
import           Device.Types
import           Foreign.C.Types        (CTime (..))
import           Haxl.Core              (GenHaxl)
import           Haxl.RedisCache        (cached, cached', get, remove, set)
import           System.Entropy         (getEntropy)
import           Text.Read              (readMaybe)
import           Web.Scotty.Haxl        ()


replaceLB :: LB.ByteString -> LB.ByteString
replaceLB = LB.fromStrict . encodeUtf8 . replace "NAN" "0" . decodeUtf8 . LB.toStrict

($>) :: GenHaxl u w a -> GenHaxl u w () -> GenHaxl u w a
io $> a = do
  !r <- io
  !_ <- a
  return r

genPingAtKey :: DeviceID -> ByteString
genPingAtKey (DeviceID devid) = fromString $ "ping_at:" ++ show devid

genDeviceKey :: DeviceID -> ByteString
genDeviceKey (DeviceID devid) = fromString $ "device:" ++ show devid

genMetricKey :: DeviceID -> ByteString
genMetricKey (DeviceID devid) = fromString $ "metric:" ++ show devid

unCacheDevice:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheDevice devid io = io $> remove redisEnv (genDeviceKey devid)

unCacheMetric:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheMetric devid io = io $> remove redisEnv (genMetricKey devid)

getDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w (Maybe Device)
getDevice devid = do
  mdev <- cached redisEnv (genDeviceKey devid) $ RawAPI.getDevice devid
  case mdev of
    Nothing -> pure Nothing
    Just dev -> do
      metric <- getLastMetric devid
      pingAt <- getPingAt devid (devCreatedAt dev)
      key <- getDevKeyById (devKeyId dev)
      pure $ Just dev { devPingAt = pingAt, devKey = key, devMetric = metric }

updateDeviceMeta
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> Meta -> GenHaxl u w Int64
updateDeviceMeta devid = updateDevice devid "meta" . decodeUtf8 . LB.toStrict . encode

updateDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> String -> Text -> GenHaxl u w Int64
updateDevice devid f = unCacheDevice devid . RawAPI.updateDevice devid f

removeDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Int64
removeDevice devid = do
  v0 <- unCacheDevice devid $ RawAPI.removeDevice devid
  v1 <- dropMetric devid ""
  v2 <- RawAPI.removeIndex Nothing (Just devid)
  return $ v0 + v1 + v2

filterMeta :: Bool -> Value -> Value -> Value
filterMeta False (Object nv) (Object ov) = Object $ KeyMap.filterWithKey (\k _ -> KeyMap.member k ov) nv
filterMeta _ nv _ = nv

getEpochTimeInt :: GenHaxl u w Int64
getEpochTimeInt = liftIO $ un . toEpochTime <$> getUnixTime
  where un :: CTime -> Int64
        un (CTime t) = t

getEpochTime :: GenHaxl u w CreatedAt
getEpochTime = CreatedAt <$> getEpochTimeInt

valueLookup :: Key.Key -> Value -> Maybe Value
valueLookup key (Object ov) = KeyMap.lookup key ov
valueLookup _ _             = Nothing

valueMember :: Key.Key -> Value -> Bool
valueMember key (Object ov) = KeyMap.member key ov
valueMember _ _             = False


online :: Value
online = object [ "state" .= ("online" :: String) ]

updateDeviceMetaValue :: (HasPSQL u, HasOtherEnv Cache u) => String -> Device -> Value -> GenHaxl u w ()
updateDeviceMetaValue "ping" Device{devID=did, devMeta=ometa} _ = do
  ct <- getEpochTime
  case valueLookup "state" ometa of
    Just (String "online") -> pure ()
    _ -> do
      void $ updateDeviceMeta did (online `union` ometa)
      void $ saveMetric did ct online

  setPingAt did ct

updateDeviceMetaValue "telemetry" Device{devID=did} v = do
  ct <- getEpochTime
  void $ saveMetric did ct v
  setPingAt did ct

updateDeviceMetaValue tp Device{devID=did, devMeta=ometa} v = do
  ct <- getEpochTime
  void $ saveMetric did ct v
  unless (valueMember "err" v) $ do
    unless (nv == ometa) $ void $ updateDeviceMeta did nv
    unless (valueMember "addr" v) $ do
      setPingAt did ct

  where nv = union (filterMeta force v ometa)
           $ if valueMember "addr" v then ometa
                                     else online `union` ometa
        force = tp == "attributes"

updateDeviceMetaByUUID :: (HasPSQL u, HasOtherEnv Cache u) => String -> UUID -> LB.ByteString -> GenHaxl u w ()
updateDeviceMetaByUUID tp (UUID uuid) meta0 = do
  devid <- getDevIdByCol "uuid" uuid
  case devid of
    Nothing -> pure ()
    Just did -> do
      mdev <- getDevice did
      case mdev of
        Nothing  -> pure ()
        Just dev -> for_ (decode meta) (updateDeviceMetaValue tp dev)

  where meta = replaceLB meta0

getPingAt :: (HasOtherEnv Cache u) => DeviceID -> CreatedAt -> GenHaxl u w CreatedAt
getPingAt did defval = fromMaybe defval <$> get redisEnv (genPingAtKey did)


setPingAt :: (HasOtherEnv Cache u) => DeviceID -> CreatedAt -> GenHaxl u w ()
setPingAt did = set redisEnv (genPingAtKey did)

getDevId :: HasPSQL u => Text -> GenHaxl u w (Maybe DeviceID)
getDevId ident
  | T.take 3 ident == "id_" = pure $ fmap DeviceID $ readMaybe $ T.unpack $ T.drop 3 ident
  | T.take 5 ident == "addr_" = getDevIdByCol "addr" $ T.drop 5 ident
  | T.take 6 ident == "token_" = getDevIdByCol "token" $ T.drop 6 ident
  | otherwise = getDevIdByCol "uuid" ident


saveMetric
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> CreatedAt -> Value -> GenHaxl u w Int64
saveMetric did createdAt =
  unCacheMetric did . saveMetricValue did createdAt

removeMetric
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> MetricID -> GenHaxl u w Int64
removeMetric did = unCacheMetric did . RawAPI.removeMetric

dropMetric
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> String -> GenHaxl u w Int64
dropMetric did = unCacheMetric did . RawAPI.dropMetric did

valueLookupTime :: Key.Key -> Value -> Maybe CreatedAt
valueLookupTime k v =
  case valueLookup k v of
    Just (Number vv) -> Just . CreatedAt $ floor vv
    Just (String vv) -> CreatedAt <$> readMaybe (T.unpack vv)
    _                -> Nothing


getValueTime :: [Key.Key] -> Value -> Maybe CreatedAt
getValueTime [] _ = Nothing
getValueTime (x:xs) v =
  case valueLookupTime x v of
    Just vv -> Just vv
    Nothing -> getValueTime xs v


saveMetricValue
  :: HasPSQL u
  => DeviceID -> CreatedAt -> Value -> GenHaxl u w Int64
saveMetricValue did createdAt (Object value) =
  sum <$> mapM (saveMetricOne did ct) (KeyMap.toList value)
  where keys = ["created_at", "updated_at", "timestamp", "time"]
        ct = fromMaybe createdAt $ getValueTime keys (Object value)
saveMetricValue did createdAt (Array value) =
  sum <$> mapM (saveMetricValue did createdAt) value
saveMetricValue _ _ _                        = return 0


saveMetricOne :: HasPSQL u => DeviceID -> CreatedAt -> (Key.Key, Value) -> GenHaxl u w Int64
saveMetricOne _ _ ("err", _) = return 0
saveMetricOne _ _ ("error", _) = return 0
saveMetricOne _ _ ("created_at", _) = return 0
saveMetricOne _ _ ("updated_at", _) = return 0
saveMetricOne _ _ ("timestamp", _) = return 0
saveMetricOne _ _ ("time", _) = return 0
saveMetricOne did createdAt (field, String "online") =
  RawAPI.saveMetric did (Key.toString field) "online" 1 createdAt
saveMetricOne did createdAt (field, String "offline") =
  RawAPI.saveMetric did (Key.toString field) "offline" 0 createdAt
saveMetricOne did createdAt (field, Number value) =
  RawAPI.saveMetric did (Key.toString field) rv fv createdAt
  where rv = show value
        fv = toRealFloat value
saveMetricOne did createdAt (field, Bool True) =
  RawAPI.saveMetric did (Key.toString field) "true" 1 createdAt
saveMetricOne did createdAt (field, Bool False) =
  RawAPI.saveMetric did (Key.toString field) "false" 0 createdAt
saveMetricOne _ _ _ = return 0


getLastMetric_ :: HasPSQL u => DeviceID -> GenHaxl u w Value
getLastMetric_ did = do
  metrics <- RawAPI.getLastMetricIdList did
  vals <- forM metrics $ \(field, mid) -> do
    metric <- RawAPI.getMetric mid
    case metric of
      Nothing -> return Null
      Just m  -> return $ object [ Key.fromString field .= metricValue m ]
  return $ foldl union Null vals

getLastMetric :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Value
getLastMetric did = do
  cached' redisEnv (genMetricKey did) $ getLastMetric_ did

toHex :: ByteString -> Text
toHex = toLower . decodeUtf8 . B16.encode

getNonZeroAddr :: IO Addr
getNonZeroAddr = do
  addr <- Addr . toHex <$> getEntropy 4
  if addr == zeroAddr then getNonZeroAddr
                      else return addr

  where zeroAddr = Addr "00000000"


randomAddr :: HasPSQL u => GenHaxl u w Addr
randomAddr = do
  addr <- liftIO getNonZeroAddr
  mdevid <- getDevIdByCol "addr" $ unAddr addr
  case mdevid of
    Nothing -> pure addr
    Just _  -> randomAddr
