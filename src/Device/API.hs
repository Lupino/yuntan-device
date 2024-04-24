{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Device.API
  ( createDevice
  , getDevice
  , countDevice
  , countDeviceByKey
  , updateDeviceMeta
  , updateDeviceToken
  , removeDevice
  , updateDeviceMetaByUUID
  , getDevKeyId
  , module X
  ) where


import           Control.Monad          (unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value (Object), decode, encode, object,
                                         (.=))
import           Data.Aeson.Helper      (union)
import qualified Data.Aeson.KeyMap      as KeyMap (filterWithKey, member)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as LB (ByteString, fromStrict, toStrict)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe)
import           Data.String            (fromString)
import           Data.Text              (Text, replace)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.UnixTime
import           Database.PSQL.Types    (HasOtherEnv, HasPSQL)
import           Device.Config          (Cache, redisEnv)
import           Device.RawAPI          as X (createTable, getDevIdByToken,
                                              getDevIdByUuid, getDevIdList,
                                              getDevIdListByKey)
import qualified Device.RawAPI          as RawAPI
import           Device.Types
import           Haxl.Core              (GenHaxl)
import           Haxl.RedisCache        (cached, cached', get, remove, set)
import           Web.Scotty.Haxl        ()


replaceLB :: LB.ByteString -> LB.ByteString
replaceLB = LB.fromStrict . encodeUtf8 . replace "NAN" "0" . decodeUtf8 . LB.toStrict

($>) :: GenHaxl u w a -> GenHaxl u w () -> GenHaxl u w a
io $> a = do
  !r <- io
  !_ <- a
  return r

genPingAtKey :: DeviceID -> ByteString
genPingAtKey devid = fromString $ "ping_at:" ++ show devid

genDeviceKey :: DeviceID -> ByteString
genDeviceKey devid = fromString $ "device:" ++ show devid

genDevKeyIdKey :: Key -> ByteString
genDevKeyIdKey key = "devkey:" <> encodeUtf8 key

genDevKeyByIdKey :: KeyID -> ByteString
genDevKeyByIdKey kid = fromString $ "devkeyid:" ++ show kid

unCacheDevice:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheDevice devid io = io $> remove redisEnv (genDeviceKey devid)

genCountKey :: String -> ByteString
genCountKey k = fromString $ "count:" ++ k

genCountKeyByKey :: KeyID -> ByteString
genCountKeyByKey kid = genCountKey ("device_name_" ++ show kid)

unCacheCount :: HasOtherEnv Cache u => String -> GenHaxl u w a -> GenHaxl u w a
unCacheCount k io = io $> remove redisEnv (genCountKey k)

unCacheCountByKey :: HasOtherEnv Cache u => KeyID -> GenHaxl u w a -> GenHaxl u w a
unCacheCountByKey kid = unCacheCount ("device_name_" ++ show kid)

createDevice :: (HasPSQL u, HasOtherEnv Cache u) => KeyID -> Token ->  GenHaxl u w DeviceID
createDevice kid t =
  unCacheCountByKey kid
  $ unCacheCount "device"
  $ RawAPI.createDevice kid t

getDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w (Maybe Device)
getDevice devid = do
  mdev <- cached redisEnv (genDeviceKey devid) $ RawAPI.getDevice devid
  case mdev of
    Nothing -> pure Nothing
    Just dev -> do
      pingAt <- getPingAt devid
      key <- getDevKeyById (devKeyId dev)
      pure $ Just dev { devPingAt = pingAt, devKey = key }

countDevice :: (HasPSQL u, HasOtherEnv Cache u) => GenHaxl u w Int64
countDevice = cached' redisEnv (genCountKey "device") RawAPI.countDevice

countDeviceByKey :: (HasPSQL u, HasOtherEnv Cache u) => KeyID -> GenHaxl u w Int64
countDeviceByKey kid =
  cached' redisEnv (genCountKeyByKey kid)
  $ RawAPI.countDeviceByKey kid

updateDeviceMeta
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> Meta -> GenHaxl u w Int64
updateDeviceMeta devid = updateDevice devid "meta" . decodeUtf8 . LB.toStrict . encode

updateDeviceToken :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> Token -> GenHaxl u w Int64
updateDeviceToken devid = updateDevice devid "token"

updateDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> String -> Text -> GenHaxl u w Int64
updateDevice devid f = unCacheDevice devid . RawAPI.updateDevice devid f

removeDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Int64
removeDevice devid = do
  dev' <- getDevice devid
  case dev' of
    Nothing -> return 0
    Just dev ->
      unCacheDevice devid
      $ unCacheCountByKey (devKeyId dev)
      $ unCacheCount "device"
      $ RawAPI.removeDevice devid

filterMeta :: Bool -> Value -> Value -> Value
filterMeta False (Object nv) (Object ov) = Object $ KeyMap.filterWithKey (\k _ -> KeyMap.member k ov) nv
filterMeta _ nv _ = nv

updateDeviceMetaByUUID :: (HasPSQL u, HasOtherEnv Cache u) => Text -> LB.ByteString -> Bool -> GenHaxl u w ()
updateDeviceMetaByUUID uuid meta0 force = do
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
              unless (KeyMap.member "err" ev) $ do
                let nv = union (filterMeta force (Object ev) ometa)
                       $ if KeyMap.member "addr" ev then ometa
                                                else online `union` ometa
                unless (nv == ometa) $ void $ updateDeviceMeta did nv
                unless (KeyMap.member "addr" ev) $ do
                  t <- liftIO $ read . show . toEpochTime <$> getUnixTime
                  void $ set redisEnv (genPingAtKey did) (t :: Int64)
            _ -> pure ()

  where online = object [ "state" .= ("online" :: String) ]
        meta = replaceLB meta0

getPingAt :: (HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Int64
getPingAt did = fromMaybe 0 <$> get redisEnv (genPingAtKey did)

getDevKeyId :: (HasPSQL u, HasOtherEnv Cache u) => Key -> GenHaxl u w KeyID
getDevKeyId key = cached' redisEnv (genDevKeyIdKey key) $ RawAPI.getDevKeyId key

getDevKeyById :: (HasPSQL u, HasOtherEnv Cache u) => KeyID -> GenHaxl u w Key
getDevKeyById kid = cached' redisEnv (genDevKeyByIdKey kid) $ RawAPI.getDevKeyById kid
