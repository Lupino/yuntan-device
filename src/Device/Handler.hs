{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Device.Handler
  ( requireDevice
  , createDeviceHandler
  , updateDeviceMetaHandler
  , updateDevicePingAtHandler
  , updateDeviceHandler
  , getDeviceListHandler
  , removeDeviceHandler
  , getDeviceHandler
  , rpcHandler

  , saveMetricHandler
  , removeMetricHandler
  , dropMetricHandler
  , getMetricListHandler
  ) where

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (lift)
import           Data.Aeson             (decode)
import           Data.Aeson.Helper      (union)
import           Data.Aeson.Result      (List (..))
import           Data.Int               (Int64)
import           Data.Maybe             (catMaybes)
import qualified Data.Text              as T (length, null, pack, splitOn)
import           Database.PSQL.Types    (From (..), HasOtherEnv, HasPSQL,
                                         OrderBy, Size (..), asc, desc)
import           Device
import           Device.Config          (Cache)
import           Device.MQTT            (MqttEnv (mAllowKeys, mKey), cacheAble,
                                         request, sendDrop)
import           Haxl.Core              (GenHaxl)
import           Network.HTTP.Types     (status500)
import           Web.Scotty.Haxl        (ActionH)
import           Web.Scotty.Trans       (addHeader, captureParam, formParam,
                                         json, raw)
import           Web.Scotty.Utils       (err, errBadRequest, errNotFound, ok,
                                         okListResult, safeFormParam,
                                         safeQueryParam)

-- :ident
apiDevice :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w (Maybe Device)
apiDevice = do
  ident <- captureParam "ident"
  lift $ do
    devid <- getDevId ident
    case devid of
      Nothing  -> pure Nothing
      Just did -> getDevice did

requireDevice :: (HasPSQL u, HasOtherEnv Cache u) => (Device -> ActionH u w ()) -> ActionH u w ()
requireDevice next = do
  device <- apiDevice
  case device of
    Just o  -> next o
    Nothing -> errNotFound "Device is not found"


checkUsed
  :: GenHaxl u w (Maybe a)
  -> String -> ActionH u w () -> ActionH u w ()
checkUsed doCheck errMsg next = do
  v <- lift doCheck
  case v of
    Nothing -> next
    Just _  -> errBadRequest errMsg


-- POST /api/devices/
createDeviceHandler
  :: (Monoid w, HasPSQL u, HasOtherEnv Cache u)
  => [Key] -> ActionH u w ()
createDeviceHandler allowKeys = do
  key <- Key <$> formParam "key"
  if key `elem` allowKeys then do
    kid <- lift $ getDevKeyId key
    token <- Token <$> formParam "token"
    addr <- lift randomAddr
    checkUsed (getDevIdByCol "token" (unToken token)) "token is already used" $ do
      devid <- lift $ createDevice kid token addr
      json =<< lift (getDevice devid)

  else errBadRequest "key is invalid"

-- POST /api/devices/:ident/token/
-- POST /api/devices/:ident/uuid/
-- POST /api/devices/:ident/addr/
-- POST /api/devices/:ident/gw_id/
updateDeviceHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => String -> Device -> ActionH u w ()
updateDeviceHandler field Device{devID = did} = do
  value <- formParam $ T.pack field
  ret <- lift $ updateDevice did field value
  resultOKOrErr ret $ "update device " ++ field ++ " failed"

-- POST /api/devices/:ident/meta/
updateDeviceMetaHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDeviceMetaHandler Device{devID = did, devMeta = ometa} = do
  meta <- formParam "meta"
  case decode meta of
    Just ev -> void (lift $ updateDeviceMeta did $ union ev ometa) >> resultOK
    Nothing -> errBadRequest "meta field is required."

-- POST /api/devices/:ident/ping_at/
updateDevicePingAtHandler :: (Monoid w, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDevicePingAtHandler Device{devID = did} = do
  pingAt <- CreatedAt <$> formParam "ping_at"
  lift $ setPingAt did pingAt
  resultOK

-- GET /api/devices/
getDeviceListHandler :: (HasPSQL u, HasOtherEnv Cache u, Monoid w) => [Key] -> ActionH u w ()
getDeviceListHandler allowKeys = do
  key <- Key <$> safeQueryParam "key" ""
  idents <- safeQueryParam "idents" ""
  gwid <- DeviceID <$> safeQueryParam "gw_id" 0
  if T.length idents > 0 then do
    devices <- lift $ do
      ids <- catMaybes <$> mapM getDevId (T.splitOn "," idents)
      catMaybes <$> mapM getDevice ids

    okListResult "devices" List
      { getFrom   = 0
      , getSize   = fromIntegral (length devices)
      , getTotal  = fromIntegral (length devices)
      , getResult = devices
      }

  else if key `elem` allowKeys then do
    kid <- lift $ getDevKeyId key
    resultDeviceList (getDevIdListByKey kid) (countDeviceByKey kid)
  else if gwid > 0 then resultDeviceList (getDevIdListByGw gwid) (countDevAddrByGw gwid)
  else resultDeviceList getDevIdList countDevice

-- DELETE /api/devices/:ident/
removeDeviceHandler :: (HasPSQL u, HasOtherEnv Cache u) => MqttEnv -> Device -> ActionH u w ()
removeDeviceHandler mqtt_ Device{devID = did, devUUID = uuid, devKey = key} = do
  void $ lift $ removeDevice did
  liftIO $ sendDrop mqtt uuid
  resultOK

  where mqtt = if key `elem` mAllowKeys mqtt_ then mqtt_ {mKey = key} else mqtt_


-- GET /api/devices/:ident/
getDeviceHandler :: Device -> ActionH u w ()
getDeviceHandler = ok "device"

resultOK :: ActionH u w ()
resultOK = ok "result" ("OK" :: String)

resultOKOrErr :: Int64 -> String -> ActionH u w ()
resultOKOrErr o m = if o > 0 then resultOK
                             else err status500 m

resultDeviceList
  :: (HasPSQL u, HasOtherEnv Cache u, Monoid w)
  => (From -> Size -> OrderBy -> GenHaxl u w [DeviceID])
  -> GenHaxl u w Int64 -> ActionH u w ()
resultDeviceList getList count = do
  from <- From <$> safeQueryParam "from" 0
  size <- Size <$> safeQueryParam "size" 10
  total <- lift count
  devices <- lift $ mapM getDevice =<< getList from size (desc "id")

  okListResult "devices" List
    { getFrom   = unFrom from
    , getSize   = unSize size
    , getTotal  = total
    , getResult = catMaybes devices
    }

rpcHandler :: (HasPSQL u, Monoid w) => MqttEnv -> Device -> ActionH u w ()
rpcHandler mqtt_ Device{devUUID = uuid, devKey = key} = do
  payload <- formParam "payload"
  tout <- min 300 <$> safeFormParam "timeout" 300
  cacheHash <- safeFormParam "cache-hash" ""
  cacheTimeout <- safeFormParam "cache-timeout" 10
  let ca = if T.null cacheHash then id else cacheAble mqtt cacheHash cacheTimeout
  r <- liftIO $ ca $ request mqtt uuid payload tout
  case r of
    Nothing -> err status500 "request timeout"
    Just v  -> do
      isjson <- safeFormParam "format" ("raw" :: String)
      when (isjson == "json") $ addHeader "Content-Type" "application/json; charset=utf-8"
      raw v

  where mqtt = if key `elem` mAllowKeys mqtt_ then mqtt_ {mKey = key} else mqtt_


-- POST /api/devices/:ident/metric/
saveMetricHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
saveMetricHandler Device{devID = did} = do
  metric <- formParam "metric"
  ct <- lift getEpochTimeInt
  createdAt <- CreatedAt <$> safeFormParam "created_at" ct
  case decode metric of
    Just ev -> void (lift $ saveMetric did createdAt ev) >> resultOK
    Nothing -> errBadRequest "metric field is required."


-- DELETE /api/devices/:ident/metric/:field/:mid/
removeMetricHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
removeMetricHandler Device{devID = did} = do
  field <- captureParam "field"
  mid <- MetricID <$> captureParam "mid"

  lift $ do
    mm <- getMetric mid
    case mm of
      Nothing -> pure ()
      Just m  ->
        when (metricField m == field && did == metricDevId m) $
          void $ removeMetric did mid

  resultOK

-- DELETE /api/devices/:ident/metric/:field/
dropMetricHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
dropMetricHandler Device{devID = did} = do
  field <- captureParam "field"

  lift $
    case field of
      "" -> pure ()
      _  -> void $ dropMetric did field
  resultOK


-- GET /api/devices/:ident/metric/:field/
getMetricListHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
getMetricListHandler Device{devID = did} = do
  from <- From <$> safeQueryParam "from" 0
  size <- Size <$> safeQueryParam "size" 10
  startedAt <- safeQueryParam "started_at" 0
  endedAt <- safeQueryParam "ended_at" 0
  field <- captureParam "field"
  s <- safeQueryParam "sort" ("asc" :: String)

  let sort = if s == "asc" then asc else desc

  total <- lift $ countMetric did field startedAt endedAt
  metrics <- lift $ mapM getMetric =<< getMetricIdList did field startedAt endedAt from size (sort "created_at")

  okListResult "data" List
    { getFrom   = unFrom from
    , getSize   = unSize size
    , getTotal  = total
    , getResult = catMaybes metrics
    }
