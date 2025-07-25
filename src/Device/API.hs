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
  , module X
  ) where


import           Control.Monad          (unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value (Object), decode, encode, object,
                                         (.=))
import           Data.Aeson.Helper      (union)
import qualified Data.Aeson.KeyMap      as KeyMap (filterWithKey, member)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Lazy   as LB (ByteString, fromStrict, toStrict)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe)
import           Data.String            (fromString)
import           Data.Text              (Text, replace, toLower)
import qualified Data.Text              as T (drop, take, unpack)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.UnixTime
import           Database.PSQL.Types    (HasOtherEnv, HasPSQL)
import           Device.Config          (Cache, redisEnv)
import           Device.RawAPI          as X (countDevAddrByGw, countDevice,
                                              countDeviceByKey, createDevice,
                                              createTable, getDevIdByCol,
                                              getDevIdList, getDevIdListByGw,
                                              getDevIdListByKey, getDevKeyById,
                                              getDevKeyId)
import qualified Device.RawAPI          as RawAPI
import           Device.Types
import           Foreign.C.Types        (CTime (..))
import           Haxl.Core              (GenHaxl)
import           Haxl.RedisCache        (cached, get, remove, set)
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

unCacheDevice:: HasOtherEnv Cache u => DeviceID -> GenHaxl u w a -> GenHaxl u w a
unCacheDevice devid io = io $> remove redisEnv (genDeviceKey devid)

getDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w (Maybe Device)
getDevice devid = do
  mdev <- cached redisEnv (genDeviceKey devid) $ RawAPI.getDevice devid
  case mdev of
    Nothing -> pure Nothing
    Just dev -> do
      pingAt <- getPingAt devid (devCreatedAt dev)
      key <- getDevKeyById (devKeyId dev)
      pure $ Just dev { devPingAt = pingAt, devKey = key }

updateDeviceMeta
  :: (HasPSQL u, HasOtherEnv Cache u)
  => DeviceID -> Meta -> GenHaxl u w Int64
updateDeviceMeta devid = updateDevice devid "meta" . decodeUtf8 . LB.toStrict . encode

updateDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> String -> Text -> GenHaxl u w Int64
updateDevice devid f = unCacheDevice devid . RawAPI.updateDevice devid f

removeDevice :: (HasPSQL u, HasOtherEnv Cache u) => DeviceID -> GenHaxl u w Int64
removeDevice devid = unCacheDevice devid $ RawAPI.removeDevice devid

filterMeta :: Bool -> Value -> Value -> Value
filterMeta False (Object nv) (Object ov) = Object $ KeyMap.filterWithKey (\k _ -> KeyMap.member k ov) nv
filterMeta _ nv _ = nv

updateDeviceMetaByUUID :: (HasPSQL u, HasOtherEnv Cache u) => UUID -> LB.ByteString -> Bool -> GenHaxl u w ()
updateDeviceMetaByUUID (UUID uuid) meta0 force = do
  devid <- getDevIdByCol "uuid" uuid
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
                  t <- liftIO $ CreatedAt . un . toEpochTime <$> getUnixTime
                  setPingAt did t
            _ -> pure ()

  where online = object [ "state" .= ("online" :: String) ]
        meta = replaceLB meta0

        un :: CTime -> Int64
        un (CTime t) = t

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
