{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Device.API
  ( createDevice
  , getDevice
  , countDevice
  , countDeviceByName
  , countDeviceByType
  , countDeviceByNameAndType
  , updateDeviceMeta
  , updateDeviceToken
  , updateDeviceType
  , updateDeviceUserName
  , removeDevice
  , updateDeviceMetaByUUID
  , module X
  ) where


import           Control.Monad          (unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value (Object), decode, encode, object,
                                         (.=))
import           Data.Aeson.Helper      (union)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B (unpack)
import qualified Data.ByteString.Lazy   as LB (ByteString, toStrict)
import qualified Data.HashMap.Strict    as HM (filterWithKey, member)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe)
import           Data.String            (fromString)
import           Data.Text              (Text, unpack)
import           Data.Text.Encoding     (decodeUtf8)
import           Data.UnixTime
import           Database.PSQL.Types    (HasOtherEnv, HasPSQL)
import           Database.Redis         (get, runRedis, set)
import           Device.Config          (Cache, redisEnv)
import           Device.RawAPI          as X (createTable, getDevIdByToken,
                                              getDevIdByUuid, getDevIdList,
                                              getDevIdListByName,
                                              getDevIdListByNameAndType,
                                              getDevIdListByType)
import qualified Device.RawAPI          as RawAPI
import           Device.Types
import           Haxl.Core              (GenHaxl, env, userEnv)
import           Haxl.RedisCache        (cached, cached', remove)
import           Text.Read              (readMaybe)
import           Web.Scotty.Haxl        ()

($>) :: GenHaxl u w a -> GenHaxl u w () -> GenHaxl u w a
io $> a = do
  !r <- io
  !_ <- a
  return r

genLastPingKey :: DeviceID -> ByteString
genLastPingKey devid = fromString $ "device_last_ping:" ++ show devid

genDeviceKey :: DeviceID -> ByteString
genDeviceKey devid = fromString $ "device:" ++ show devid

unCacheDevice:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheDevice devid io = io $> remove redisEnv (genDeviceKey devid)

genCountKey :: String -> ByteString
genCountKey k = fromString $ "count:" ++ k

genCountKeyByName :: UserName -> ByteString
genCountKeyByName un = genCountKey ("device_name_" ++ unpack un)

genCountKeyByType :: Type -> ByteString
genCountKeyByType tp = genCountKey ("device_type_" ++ unpack tp)

genCountKeyByNameAndType :: UserName -> Type -> ByteString
genCountKeyByNameAndType un tp = genCountKey ("device_name_" ++ unpack un ++ "_type_" ++ unpack tp)

unCacheCount :: HasOtherEnv Cache u => String -> GenHaxl u w a -> GenHaxl u w a
unCacheCount k io = io $> remove redisEnv (genCountKey k)

unCacheCountByName :: HasOtherEnv Cache u => UserName -> GenHaxl u w a -> GenHaxl u w a
unCacheCountByName un = unCacheCount ("device_name_" ++ unpack un)

unCacheCountByType :: HasOtherEnv Cache u => Type -> GenHaxl u w a -> GenHaxl u w a
unCacheCountByType tp = unCacheCount ("device_type_" ++ unpack tp)

unCacheCountByNameAndType :: HasOtherEnv Cache u => UserName -> Type -> GenHaxl u w a -> GenHaxl u w a
unCacheCountByNameAndType un tp = unCacheCount ("device_name_" ++ unpack un ++ "_type_" ++ unpack tp)

createDevice :: (HasPSQL u, HasOtherEnv Cache u) => UserName -> Token -> Type ->  GenHaxl u w DeviceID
createDevice un t tp =
  unCacheCountByNameAndType un tp
  $ unCacheCountByType tp
  $ unCacheCountByName un
  $ unCacheCount "device"
  $ RawAPI.createDevice un t tp

getDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w (Maybe Device)
getDevice devid = cached redisEnv (genDeviceKey devid) $ RawAPI.getDevice devid

countDevice :: (HasPSQL u, HasOtherEnv Cache u) => GenHaxl u w Int64
countDevice = cached' redisEnv (genCountKey "device") RawAPI.countDevice

countDeviceByName :: (HasPSQL u, HasOtherEnv Cache u) => UserName -> GenHaxl u w Int64
countDeviceByName un =
  cached' redisEnv (genCountKeyByName un)
  $ RawAPI.countDeviceByName un

countDeviceByType :: (HasPSQL u, HasOtherEnv Cache u) => Type -> GenHaxl u w Int64
countDeviceByType tp =
  cached' redisEnv (genCountKeyByType tp)
  $ RawAPI.countDeviceByType tp

countDeviceByNameAndType :: (HasPSQL u, HasOtherEnv Cache u) => UserName -> Type -> GenHaxl u w Int64
countDeviceByNameAndType un tp =
  cached' redisEnv (genCountKeyByNameAndType un tp)
  $ RawAPI.countDeviceByNameAndType un tp

updateDeviceMeta
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> Meta -> GenHaxl u w Int64
updateDeviceMeta devid = updateDevice devid "meta" . decodeUtf8 . LB.toStrict . encode

updateDeviceType :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> Type -> GenHaxl u w Int64
updateDeviceType devid tp = do
  dev' <- getDevice devid
  case dev' of
    Nothing -> return 0
    Just dev ->
      unCacheCountByType (devType dev)
      $ unCacheCountByType tp
      $ unCacheCountByNameAndType (devUserName dev) (devType dev)
      $ unCacheCountByNameAndType (devUserName dev) tp
      $ updateDevice devid "type" tp

updateDeviceToken :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> Token -> GenHaxl u w Int64
updateDeviceToken devid = updateDevice devid "token"

updateDeviceUserName :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> UserName -> GenHaxl u w Int64
updateDeviceUserName devid un = do
  dev' <- getDevice devid
  case dev' of
    Nothing -> return 0
    Just dev ->
      unCacheCountByName (devUserName dev)
      $ unCacheCountByNameAndType (devUserName dev) (devType dev)
      $ updateDevice devid "username" un

updateDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> String -> Text -> GenHaxl u w Int64
updateDevice devid f = unCacheDevice devid . RawAPI.updateDevice devid f

removeDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Int64
removeDevice devid = do
  dev' <- getDevice devid
  case dev' of
    Nothing -> return 0
    Just dev ->
      unCacheDevice devid
      $ unCacheCountByType (devType dev)
      $ unCacheCountByName (devUserName dev)
      $ unCacheCountByNameAndType (devUserName dev) (devType dev)
      $ unCacheCount "device"
      $ RawAPI.removeDevice devid

filterMeta :: Bool -> Value -> Value -> Value
filterMeta True (Object nv) (Object ov) = Object $ HM.filterWithKey (\k _ -> HM.member k ov) nv
filterMeta _ nv _ = nv

updateDeviceMetaByUUID :: (HasPSQL u, HasOtherEnv Cache u) => Text -> LB.ByteString -> Bool -> GenHaxl u w ()
updateDeviceMetaByUUID uuid meta force = do
  devid <- getDevIdByUuid uuid
  case devid of
    Nothing -> pure ()
    Just did -> do
      dev <- getDevice did
      case dev of
        Nothing -> pure ()
        Just Device{devMeta = ometa} ->
          case decode meta of
            Just (Object ev) ->
              unless (HM.member "err" ev) $ do
                let nv = union (filterMeta force (Object ev) ometa)
                       $ if HM.member "addr" ev then ometa
                                                else union online ometa
                unless (nv == ometa) $ void $ updateDeviceMeta did nv
                unless (HM.member "addr" ev || HM.member "state" ev) $ do
                  mconn <- redisEnv <$> env userEnv
                  case mconn of
                    Nothing -> pure ()
                    Just conn -> liftIO $ do
                      t <- liftIO $ show . toEpochTime <$> getUnixTime
                      void $ runRedis conn $ set (genLastPingKey did) $ fromString t
            _ -> pure ()

  where online = object [ "state" .= ("online" :: String) ]

getLastPing :: (HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Int64
getLastPing did = do
  mconn <- redisEnv <$> env userEnv
  case mconn of
    Nothing -> pure 0
    Just conn -> liftIO $ do
      r <- runRedis conn $ get (genLastPingKey did)
      case r of
        Right (Just t) -> pure . fromMaybe 0 . readMaybe $ B.unpack t
        _              -> pure 0
