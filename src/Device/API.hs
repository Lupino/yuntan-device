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

  , getMeta

  , saveCard
  , removeCard

  , saveIndex
  , removeIndex

  , module X
  ) where


import           Control.Monad          (forM, unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value (..), decode, encode, object,
                                         (.=))
import           Data.Aeson.Helper      (union)
import qualified Data.Aeson.Key         as Key (Key, fromText, toText)
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
import           Database.PSQL          (Column, HasOtherEnv, HasPSQL, pageNone)
import           Device.Config          (Cache, redisEnv)
import           Device.RawAPI          as X (countDevAddrByGw, countDevice,
                                              countDeviceByKey, countIndex,
                                              countMetric, createDevice,
                                              createTable, getCard,
                                              getDevIdByCol, getDevIdList,
                                              getDevIdListByGw,
                                              getDevIdListByKey, getDevKeyById,
                                              getDevKeyId, getIndexDevIdList,
                                              getIndexNameId, getIndexNameId_,
                                              getMetric, getMetricIdList,
                                              removeIndexName)
import qualified Device.RawAPI          as RawAPI
import           Device.Types
import qualified Device.Util            as Util (getEpochTime)
import           Haxl.Core              (GenHaxl)
import           Haxl.RedisCache        (cached, cached', get, hdel, hget,
                                         hgetallKV, hgetallV, hset, remove, set)
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

genCardsKey :: DeviceID -> ByteString
genCardsKey (DeviceID devid) = fromString $ "cards:" ++ show devid

genIndexKey :: DeviceID -> ByteString
genIndexKey (DeviceID devid) = fromString $ "index:" ++ show devid

genMetaKey :: DeviceID -> ByteString
genMetaKey (DeviceID devid) = fromString $ "meta:" ++ show devid

unCacheDevice:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheDevice devid io = io $> remove redisEnv (genDeviceKey devid)

unCacheMetric:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheMetric devid io = io $> remove redisEnv (genMetricKey devid)

unCacheCards:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheCards devid io = io $> remove redisEnv (genCardsKey devid)

unCacheIndex:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheIndex devid io = io $> remove redisEnv (genIndexKey devid)

unCacheMeta:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheMeta devid io = io $> remove redisEnv (genMetaKey devid)

getDevice :: (HasPSQL u, HasOtherEnv Cache u) => Bool -> DeviceID -> GenHaxl u w (Maybe Device)
getDevice False devid = cached redisEnv (genDeviceKey devid) $ RawAPI.getDevice devid
getDevice True devid = do
  mdev <- getDevice False devid
  case mdev of
    Nothing -> pure Nothing
    Just dev -> do
      metric <- getLastMetric devid
      cards <- getCards devid
      pingAt <- getPingAt devid (devCreatedAt dev)
      key <- getDevKeyById (devKeyId dev)
      index <- getIndexList devid
      meta <- getMeta devid
      pure $ Just dev
        { devPingAt = pingAt
        , devKey = key
        , devMetric = metric
        , devCards = cards
        , devIndex = index
        , devMeta = meta
        }

updateDeviceMeta
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> Bool -> Meta -> GenHaxl u w Int64
updateDeviceMeta did True meta =
  unCacheMeta did $ updateDeviceMetaRaw did meta
updateDeviceMeta did False meta = do
  newMeta <- (meta `union`) <$> getMeta did
  cacheMeta did newMeta
  updateDeviceMetaRaw did newMeta

updateDeviceMetaRaw :: HasPSQL u => DeviceID -> Meta -> GenHaxl u w Int64
updateDeviceMetaRaw did =
  RawAPI.updateDevice did "meta" . decodeUtf8 . LB.toStrict . encode

updateDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> Column -> Text -> GenHaxl u w Int64
updateDevice devid f = unCacheDevice devid . RawAPI.updateDevice devid f

removeDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Int64
removeDevice devid = do
  v0 <- unCacheDevice devid $ unCacheMeta devid $ RawAPI.removeDevice devid
  v1 <- dropMetric devid ""
  v2 <- RawAPI.removeIndex Nothing (Just devid)
  v3 <- dropCards devid
  return $ v0 + v1 + v2 + v3

cacheMeta :: HasOtherEnv Cache u => DeviceID -> Meta -> GenHaxl u w ()
cacheMeta did (Object kv) =
  mapM_ (\(f, v) -> hset redisEnv k (k2b f) v) $ KeyMap.toList kv
  where k = genMetaKey did
        k2b = encodeUtf8 . Key.toText
cacheMeta _ _ = pure ()

getMeta :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Meta
getMeta did = do
  meta <- hgetallKV redisEnv (genMetaKey did)
  case meta of
    (Object _) -> pure meta
    _ -> do
      mdev <- RawAPI.getDevice did
      case mdev of
        Nothing -> pure meta
        Just dev -> do
          cacheMeta did $ devMeta dev
          pure $ devMeta dev

filterMeta :: Bool -> Value -> Value -> Value
filterMeta False (Object nv) (Object ov) = Object $ KeyMap.filterWithKey (\k _ -> KeyMap.member k ov) nv
filterMeta _ nv _ = nv

getEpochTime :: GenHaxl u w CreatedAt
getEpochTime = CreatedAt <$> Util.getEpochTime

valueLookup :: Key.Key -> Value -> Maybe Value
valueLookup key (Object ov) = KeyMap.lookup key ov
valueLookup _ _             = Nothing

valueMember :: Key.Key -> Value -> Bool
valueMember key (Object ov) = KeyMap.member key ov
valueMember _ _             = False


online :: Value
online = object [ "state" .= ("online" :: String) ]

updateDeviceMetaValue :: (HasPSQL u, HasOtherEnv Cache u) => String -> Device -> Value -> GenHaxl u w ()
updateDeviceMetaValue "ping" Device{devID=did} _ = do
  ct <- getEpochTime
  ometa <- getMeta did
  case valueLookup "state" ometa of
    Just (String "online") -> pure ()
    _ -> do
      void $ updateDeviceMeta did False (online `union` ometa)
      void $ saveMetric did ct online

  setPingAt did ct

updateDeviceMetaValue "telemetry" Device{devID=did} v = do
  ct <- getEpochTime
  void $ saveMetric did ct v
  setPingAt did ct

updateDeviceMetaValue tp Device{devID=did} v = do
  ct <- getEpochTime
  ometa <- getMeta did
  void $ saveMetric did ct v
  unless (valueMember "err" v) $ do
    let nv = merge ometa
    unless (nv == ometa) $ void $ updateDeviceMeta did False nv
    unless (valueMember "addr" v) $ do
      setPingAt did ct

  where merge ometa =
          union (filterMeta force v ometa)
          $ if valueMember "addr" v then ometa
                                    else online `union` ometa
        force = tp == "attributes"

updateDeviceMetaByUUID :: (HasPSQL u, HasOtherEnv Cache u) => String -> UUID -> LB.ByteString -> GenHaxl u w ()
updateDeviceMetaByUUID tp (UUID uuid) meta0 = do
  devid <- getDevIdByCol "uuid" uuid
  case devid of
    Nothing -> pure ()
    Just did -> do
      mdev <- getDevice False did
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


removeMetric
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> MetricID -> GenHaxl u w Int64
removeMetric did = unCacheMetric did . RawAPI.removeMetric

dropMetric
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> Param -> GenHaxl u w Int64
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


saveMetric
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> CreatedAt -> Value -> GenHaxl u w Int64
saveMetric did createdAt (Object value) =
  sum <$> mapM (saveMetricOne did ct) (KeyMap.toList value)
  where keys = ["created_at", "updated_at", "timestamp", "time"]
        ct = fromMaybe createdAt $ getValueTime keys (Object value)
saveMetric did createdAt (Array value) =
  sum <$> mapM (saveMetric did createdAt) value
saveMetric _ _ _                        = return 0

key2param :: Key.Key -> Param
key2param = Param . Key.toText

saveMetricOne
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> CreatedAt -> (Key.Key, Value) -> GenHaxl u w Int64
saveMetricOne _ _ ("err", _) = return 0
saveMetricOne _ _ ("error", _) = return 0
saveMetricOne _ _ ("created_at", _) = return 0
saveMetricOne _ _ ("updated_at", _) = return 0
saveMetricOne _ _ ("timestamp", _) = return 0
saveMetricOne _ _ ("time", _) = return 0
saveMetricOne _ _ ("verified", _) = return 0
saveMetricOne _ _ ("crc", _) = return 0
saveMetricOne _ _ ("modbus", _) = return 0
saveMetricOne _ _ ("modbus_state", _) = return 0
saveMetricOne did createdAt (param, String "online") =
  saveMetricRaw did (key2param param) "online" 1 createdAt
saveMetricOne did createdAt (param, String "offline") =
  saveMetricRaw did (key2param param) "offline" 0 createdAt
saveMetricOne did createdAt (param, Number value) =
  saveMetricRaw did (key2param param) rv fv createdAt
  where rv = show value
        fv = toRealFloat value
saveMetricOne did createdAt (param, Bool True) =
  saveMetricRaw did (key2param param) "true" 1 createdAt
saveMetricOne did createdAt (param, Bool False) =
  saveMetricRaw did (key2param param) "false" 0 createdAt
saveMetricOne _ _ _ = return 0

saveMetricRaw
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> Param -> String -> Float -> CreatedAt -> GenHaxl u w Int64
saveMetricRaw did param sval val ct = do
  oval <- hget redisEnv k f
  if oval == Just val then
    pure 0
  else do
    hset redisEnv k f val
    RawAPI.saveMetric did param sval val ct
  where k = genMetricKey did
        f = encodeUtf8 (unParam param)


getLastMetric_ :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Value
getLastMetric_ did = do
  metrics <- RawAPI.getLastMetricIdList did
  vals <- forM metrics $ \(Param param, mid) -> do
    metric <- RawAPI.getMetric mid
    case metric of
      Nothing -> return Null
      Just m  -> do
        hset redisEnv k (encodeUtf8 param) (metricValue m)
        return $ object [ Key.fromText param .= metricValue m ]
  return $ foldl union Null vals

  where k = genMetricKey did

getLastMetric :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Value
getLastMetric did = do
  value <- hgetallKV redisEnv (genMetricKey did)
  if value == Null then getLastMetric_ did
                   else pure value


saveCard
  :: (HasPSQL u, HasOtherEnv Cache u)
  => Bool -> DeviceID -> Param -> Meta -> GenHaxl u w (Maybe Card)
saveCard replaceMeta did param meta = do
  mCardId <- RawAPI.getCardId did param
  case mCardId of
      Nothing -> do
        cardId <- RawAPI.createCard did param meta
        mCard <- getCard cardId
        mapM_ cacheCard mCard
        pure mCard
      Just cardId -> do
        mCard <- getCard cardId
        case mCard of
          Nothing -> pure Nothing
          Just card -> do
            let newMeta = if replaceMeta then meta else  meta `union` cardMeta card
                newCard = card { cardMeta = newMeta }
            void $ RawAPI.updateCardMeta cardId newMeta
            cacheCard newCard
            pure $ Just newCard

removeCard
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> Param -> GenHaxl u w Int64
removeCard did param = do
  mCardId <- RawAPI.getCardId did param
  case mCardId of
    Nothing     -> pure 0
    Just cardId -> do
      hdel redisEnv k [f]
      RawAPI.removeCard cardId
  where k = genCardsKey did
        f = encodeUtf8 $ unParam param


dropCards
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> GenHaxl u w Int64
dropCards did = unCacheCards did $ RawAPI.dropCards did

getCards :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w [Card]
getCards did = do
  cards <- hgetallV redisEnv (genCardsKey did)
  if null cards then getCardsAndCache did
                else pure cards

getCardsAndCache :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w [Card]
getCardsAndCache did = do
  cards <- RawAPI.getCards did
  mapM_ cacheCard cards
  return cards

cacheCard :: HasOtherEnv Cache u => Card -> GenHaxl u w ()
cacheCard card =
  hset redisEnv k (encodeUtf8 param) card
  where k = genCardsKey $ cardDevId card
        param = unParam $ cardParam card

saveIndex :: (HasPSQL u, HasOtherEnv Cache u) => IndexNameId -> DeviceID -> GenHaxl u w Int64
saveIndex iid did = unCacheIndex did $ RawAPI.saveIndex iid did

getIndexList :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w [Index]
getIndexList did = cached' redisEnv (genIndexKey did) $ RawAPI.getIndexList did

removeIndex :: (HasPSQL u, HasOtherEnv Cache u) => Maybe IndexNameId -> Maybe DeviceID -> GenHaxl u w Int64
removeIndex Nothing Nothing = pure 0
removeIndex mIid (Just did) = unCacheIndex did $ RawAPI.removeIndex mIid (Just did)
removeIndex (Just iid) Nothing = do
  dids <- getIndexDevIdList [iid] pageNone
  mapM_ (remove redisEnv . genIndexKey) dids
  RawAPI.removeIndex (Just iid) Nothing

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
