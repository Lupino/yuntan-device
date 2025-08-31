{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

  , saveCardHandler
  , removeCardHandler

  , saveIndexHandler
  , removeIndexHandler
  , dropDeviceIndexHandler
  , dropIndexHandler

  , emqxAclReqHandler
  , emqxSuperReqHandler
  , emqxAuthReqHandler
  ) where

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (lift)
import           Data.Aeson             (decode)
import           Data.Aeson.Helper      (union)
import           Data.Aeson.Result      (List (..))
import           Data.Char              (toUpper)
import           Data.Int               (Int64)
import           Data.Maybe             (catMaybes)
import           Data.String            (fromString)
import qualified Data.Text              as T (length, null, pack, splitOn,
                                              unpack)
import           Data.UUID              (toText)
import           Data.UUID.V4           (nextRandom)
import           Database.PSQL          (Column (..), From (..), HasOtherEnv,
                                         HasPSQL, Page (..), Size (..), asc,
                                         desc)
import           Device
import           Device.Config          (Cache, EmqxAdminConfig (..),
                                         EmqxAuthConfig (..))
import           Device.MQTT            (MqttEnv (mKey), cacheAble, request,
                                         sendDrop)
import           Device.Util            (getEpochTime, parseIndexName)
import           Haxl.Core              (GenHaxl)
import           Network.HTTP.Types     (status400, status500)
import           Web.Scotty.Haxl        (ActionH)
import           Web.Scotty.Trans       (addHeader, captureParam, formParam,
                                         json, raw, text)
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
      Just did -> getDevice False did

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
    checkUsed (getDevIdByCol "token" (unToken token)) "token is already used" $ do
      uuid <- liftIO $ UUID . toText <$> nextRandom
      addr <- lift randomAddr
      devid <- lift $ createDevice kid token addr uuid
      json =<< lift (getDevice True devid)

  else errBadRequest "key is invalid"

-- POST /api/devices/:ident/token/
-- POST /api/devices/:ident/uuid/
-- POST /api/devices/:ident/addr/
-- POST /api/devices/:ident/gw_id/
updateDeviceHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Column -> Device -> ActionH u w ()
updateDeviceHandler col Device{devID = did} = do
  value <- formParam $ T.pack $ unColumn col
  ret <- lift $ updateDevice did col value
  resultOKOrErr ret $ "update device " ++ unColumn col ++ " failed"


parseBool :: String -> Bool
parseBool v = map toUpper v == "TRUE"

-- POST /api/devices/:ident/meta/
updateDeviceMetaHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDeviceMetaHandler Device{devID = did, devMeta = ometa} = do
  meta <- formParam "meta"
  replaceMeta <- parseBool <$> safeFormParam "replace" "false"
  case decode meta of
    Just ev -> do
      let newEv = if replaceMeta then ev else ev `union` ometa
      void (lift $ updateDeviceMeta did newEv) >> resultOK
    Nothing -> errBadRequest "meta is required."

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
  indexNames <- parseIndexName <$> safeQueryParam "index_name" ""
  idents <- safeQueryParam "idents" ""
  gwid <- DeviceID <$> safeQueryParam "gw_id" 0
  if T.length idents > 0 then do
    devices <- lift $ do
      ids <- catMaybes <$> mapM getDevId (T.splitOn "," idents)
      catMaybes <$> mapM (getDevice True) ids

    okListResult "devices" List
      { getFrom   = 0
      , getSize   = fromIntegral (length devices)
      , getTotal  = fromIntegral (length devices)
      , getResult = devices
      }

  else if key /= "" then
    if key `elem` allowKeys then do
      kid <- lift $ getDevKeyId key
      resultDeviceList (getDevIdListByKey kid) (countDeviceByKey kid)
    else
      errBadRequest "key is not exists"
  else if not (null indexNames) then do
    nids <- lift $ catMaybes <$> mapM getIndexNameId_ indexNames
    if null nids then
      errBadRequest "index_name is invalid"
    else
      resultDeviceList (getIndexDevIdList nids) (countIndex nids Nothing)
  else if gwid > 0 then resultDeviceList (getDevIdListByGw gwid) (countDevAddrByGw gwid)
  else resultDeviceList getDevIdList countDevice

-- DELETE /api/devices/:ident/
removeDeviceHandler :: (HasPSQL u, HasOtherEnv Cache u) => MqttEnv -> Device -> ActionH u w ()
removeDeviceHandler mqtt_ Device{devID = did, devUUID = uuid, devKeyId = keyId} = do
  void $ lift $ removeDevice did
  mk <- lift $ getDevKeyById keyId
  case mk of
    Nothing -> pure ()
    Just k  -> liftIO $ sendDrop mqtt_ {mKey = k} uuid
  resultOK


-- GET /api/devices/:ident/
getDeviceHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
getDeviceHandler Device { devID = did } = do
  lift (getDevice True did) >>= ok "device"

resultOK :: ActionH u w ()
resultOK = ok "result" ("OK" :: String)

resultOKOrErr :: Int64 -> String -> ActionH u w ()
resultOKOrErr o m = if o > 0 then resultOK
                             else err status500 m

resultDeviceList
  :: (HasPSQL u, HasOtherEnv Cache u, Monoid w)
  => (Page -> GenHaxl u w [DeviceID])
  -> GenHaxl u w Int64 -> ActionH u w ()
resultDeviceList getList count = do
  from <- From <$> safeQueryParam "from" 0
  size <- Size <$> safeQueryParam "size" 10
  total <- lift count
  devices <- lift $ mapM (getDevice True) =<< getList Page { pageFrom = from, pageSize = size, pageOrder = desc "id" }

  okListResult "devices" List
    { getFrom   = unFrom from
    , getSize   = unSize size
    , getTotal  = total
    , getResult = catMaybes devices
    }

rpcHandler :: (HasPSQL u, Monoid w) => MqttEnv -> Device -> ActionH u w ()
rpcHandler mqtt_ Device{devUUID = uuid, devKeyId = keyId} = do
  payload <- formParam "payload"
  tout <- min 300 <$> safeFormParam "timeout" 300
  cacheHash <- safeFormParam "cache-hash" ""
  cacheTimeout <- safeFormParam "cache-timeout" 10
  mk <- lift $ getDevKeyById keyId
  case mk of
    Nothing -> err status500 "invalid device"
    Just k -> do
      let mqtt = mqtt_ {mKey = k}
          ca = if T.null cacheHash then id else cacheAble mqtt cacheHash cacheTimeout
      r <- liftIO $ ca $ request mqtt uuid payload tout
      case r of
        Nothing -> err status500 "request timeout"
        Just v  -> do
          isjson <- safeFormParam "format" ("raw" :: String)
          when (isjson == "json") $
            addHeader "Content-Type" "application/json; charset=utf-8"
          raw v


-- POST /api/devices/:ident/metric/
saveMetricHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
saveMetricHandler Device{devID = did} = do
  metric <- formParam "metric"
  ct <- getEpochTime
  createdAt <- CreatedAt <$> safeFormParam "created_at" ct
  case decode metric of
    Just ev -> void (lift $ saveMetric did createdAt ev) >> resultOK
    Nothing -> errBadRequest "metric is required."


-- DELETE /api/devices/:ident/metric/:param/:mid/
removeMetricHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
removeMetricHandler Device{devID = did} = do
  param <- Param <$> captureParam "param"
  mid <- MetricID <$> captureParam "mid"

  lift $ do
    mm <- getMetric mid
    case mm of
      Nothing -> pure ()
      Just m  ->
        when (metricParam m == param && did == metricDevId m) $
          void $ removeMetric did mid

  resultOK

-- DELETE /api/devices/:ident/metric/:param/
dropMetricHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
dropMetricHandler Device{devID = did} = do
  param <- Param <$> captureParam "param"

  lift $
    case param of
      "" -> pure ()
      _  -> void $ dropMetric did param
  resultOK


-- GET /api/devices/:ident/metric/:param/
getMetricListHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
getMetricListHandler Device{devID = did} = do
  from <- From <$> safeQueryParam "from" 0
  size <- Size <$> safeQueryParam "size" 10
  startedAt <- safeQueryParam "started_at" 0
  endedAt <- safeQueryParam "ended_at" 0
  param <- Param <$> captureParam "param"
  s <- safeQueryParam "sort" ("asc" :: String)

  let sort = if s == "asc" then asc else desc
      page = Page { pageFrom = from, pageSize = size, pageOrder = sort "created_at" }

  total <- lift $ countMetric did param startedAt endedAt
  metrics <- lift $ mapM getMetric =<< getMetricIdList did param startedAt endedAt page

  okListResult "data" List
    { getFrom   = unFrom from
    , getSize   = unSize size
    , getTotal  = total
    , getResult = catMaybes metrics
    }


-- POST /api/devices/:ident/cards/
saveCardHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
saveCardHandler Device{devID = did} = do
  param <- Param <$> formParam "param"
  meta <- formParam "meta"
  replaceMeta <- parseBool <$> safeFormParam "replace" "false"
  case decode meta of
    Just ev -> do
      cardId <- lift $ saveCard replaceMeta did param ev
      json =<< lift (runWithEnv $ getCard cardId)
    Nothing -> errBadRequest "meta is required."


-- DELETE /api/devices/:ident/cards/:param/
removeCardHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
removeCardHandler Device{devID = did} = do
  param <- Param <$> captureParam "param"
  void $ lift $ removeCard did param
  resultOK


-- POST /api/devices/:ident/index/
saveIndexHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
saveIndexHandler Device{devID = did} = do
  indexName <- IndexName <$> formParam "index_name"
  lift $ do
    nid <- getIndexNameId indexName
    void $ saveIndex nid did
  resultOK


-- POST /api/devices/:ident/index/delete/
removeIndexHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
removeIndexHandler Device{devID = did} = do
  indexName <- IndexName <$> formParam "index_name"
  lift $ do
    mnid <- getIndexNameId_ indexName
    case mnid of
      Nothing -> pure ()
      _       -> void $ removeIndex mnid (Just did)
  resultOK


-- POST /api/devices/:ident/index/drop/
dropDeviceIndexHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
dropDeviceIndexHandler Device{devID = did} = do
  lift $ void $ removeIndex Nothing (Just did)
  resultOK


-- POST /api/index/drop/
dropIndexHandler :: (Monoid w, HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
dropIndexHandler = do
  indexName <- IndexName <$> formParam "index_name"
  lift $ do
    mnid <- getIndexNameId_ indexName
    case mnid of
      Nothing -> pure ()
      Just nid -> do
        void $ removeIndexName nid
        void $ removeIndex mnid Nothing
  resultOK

emqxSuperReqHandler :: ActionH u w ()
emqxSuperReqHandler = text "ok"

emqxAclReqHandler :: ActionH u w ()
emqxAclReqHandler = text "ok"


lookupEmqxUser
  :: (HasPSQL u, HasOtherEnv Cache u)
  => EmqxAuthConfig -> String -> String -> ActionH u w (Maybe EmqxUser)
lookupEmqxUser EmqxAuthConfig {..} key token
  | emqxSuperAdmin == key =
    if emqxSuperPassword == token then
      pure $ Just EmqxSuperAdmin
    else pure Nothing
  | otherwise =
    case getEmqxAdmin emqxAdminList of
      Just _ -> pure $ Just $ EmqxAdmin $ EmqxMountPoint $ "/" ++ key
      Nothing -> lift $ do
        mdid <- getDevIdByCol "token" $ T.pack token
        case mdid of
          Nothing -> pure Nothing
          Just did -> do
            mdev <- getDevice False did
            case mdev of
              Nothing -> pure Nothing
              Just dev  -> do
                mDkey <- getDevKeyById (devKeyId dev)
                case mDkey of
                  Nothing -> pure Nothing
                  Just dk ->
                    if dk == fromString key then
                      pure $ Just $ EmqxNormal $ devPoint dk dev
                    else
                      pure Nothing

  where getEmqxAdmin :: [EmqxAdminConfig] -> Maybe EmqxAdminConfig
        getEmqxAdmin [] = Nothing
        getEmqxAdmin (x:xs)
          | emqxAdminKey x == key && emqxAdminPassword x == token = Just x
          | otherwise = getEmqxAdmin xs

        devPoint :: Key -> Device -> EmqxMountPoint
        devPoint dk Device {..} = EmqxMountPoint $ T.unpack $ "/" <> k <> "/" <> u
          where k = unKey dk
                u = unUUID devUUID

emqxAuthReqHandler
  :: (Monoid w, HasPSQL u, HasOtherEnv Cache u)
  => EmqxAuthConfig -> ActionH u w ()
emqxAuthReqHandler config = do
  key <- formParam "username"
  token <- formParam "password"
  r <- lookupEmqxUser config key token
  case r of
    Nothing -> err status400 "no auth"
    Just u  -> json u
