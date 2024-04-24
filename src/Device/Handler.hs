{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Device.Handler
  ( requireDevice
  , requireOwner
  , createDeviceHandler
  , updateDeviceMetaHandler
  , updateDeviceTokenHandler
  , updateDeviceUserNameHandler
  , getDeviceListHandler
  , getDeviceListByNameHandler
  , removeDeviceHandler
  , getDeviceHandler
  , rpcHandler
  , addKey
  ) where

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (lift)
import           Data.Aeson             (decode)
import           Data.Aeson.Helper      (union)
import           Data.Aeson.Result      (List (..))
import           Data.Int               (Int64)
import           Data.Maybe             (catMaybes)
import qualified Data.Text              as T (Text, null, unpack)
import qualified Data.Text.Lazy         as LT (toStrict)
import           Data.UUID              (fromText)
import           Database.PSQL.Types    (From (..), HasOtherEnv, HasPSQL,
                                         OrderBy, Size (..), desc)
import           Device
import           Device.Config          (Cache)
import           Device.MQTT            (MqttEnv (mAllowKeys, mKey), cacheAble,
                                         request, sendDrop)
import           Haxl.Core              (GenHaxl)
import           Network.HTTP.Types     (status403, status500)
import           Web.Scotty.Haxl        (ActionH)
import           Web.Scotty.Trans       (addHeader, captureParam, formParam,
                                         header, json, raw)
import           Web.Scotty.Utils       (err, errBadRequest, errNotFound, ok,
                                         okListResult, safeFormParam,
                                         safeQueryParam)

-- :uuidOrToken
apiDevice :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w (Maybe Device)
apiDevice = do
  uuidOrToken <- captureParam "uuidOrToken"
  let f = case fromText uuidOrToken of
            Just _  -> getDevIdByUuid
            Nothing -> getDevIdByToken

  lift $ do
    devid <- f uuidOrToken
    case devid of
      Nothing  -> pure Nothing
      Just did -> getDevice did


getKey :: Monoid w => ActionH u w (Maybe T.Text)
getKey = do
  hv0 <- header "X-REQUEST-KEY"
  case hv0 of
    Just key -> pure . Just $ LT.toStrict key
    Nothing -> do
      hv1 <- safeQueryParam "key" ""
      case hv1 of
        "" -> pure Nothing
        _  -> pure $ Just hv1


addKey :: Monoid w => ActionH u w () -> ActionH u w ()
addKey next = do
  mmKey <- getKey
  case mmKey of
    Nothing  -> pure ()
    Just key -> lift . setTablePrefix $ T.unpack key
  next

requireDevice :: (HasPSQL u, HasOtherEnv Cache u) => (Device -> ActionH u w ()) -> ActionH u w ()
requireDevice next = do
  device <- apiDevice
  case device of
    Just o  -> next o
    Nothing -> errNotFound "Device is not found"

requireOwner :: (Device -> ActionH u w ()) -> Device -> ActionH u w ()
requireOwner next device = do
  username <- captureParam "username"
  if devUserName device == username then next device
                                    else err status403 "no permission"

-- POST /api/devices/
-- POST /api/users/:username/devices/
createDeviceHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
createDeviceHandler = do
  username <- formParam "username"
  token <- formParam "token"
  olddevid <- lift $ getDevIdByToken token
  case olddevid of
    Just _ -> errBadRequest "token is already used."
    Nothing -> do
      devid <- lift $ createDevice username token
      json =<< lift (getDevice devid)

-- POST /api/devices/:uuidOrToken/token/
-- POST /api/users/:username/devices/:uuidOrToken/token/
updateDeviceTokenHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDeviceTokenHandler Device{devID = did} = do
  token <- formParam "token"
  ret <- lift $ updateDeviceToken did token
  resultOKOrErr ret "update device token failed"

-- POST /api/devices/:uuidOrToken/meta/
-- POST /api/users/:username/devices/:uuidOrToken/meta/
updateDeviceMetaHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDeviceMetaHandler Device{devID = did, devMeta = ometa} = do
  meta <- formParam "meta"
  case decode meta of
    Just ev -> void (lift $ updateDeviceMeta did $ union ev ometa) >> resultOK
    Nothing -> errBadRequest "meta filed is required."

-- POST /api/devices/:uuidOrToken/username/
updateDeviceUserNameHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDeviceUserNameHandler Device{devID = did} = do
  un <- formParam "username"
  ret <- lift $ updateDeviceUserName did un
  resultOKOrErr ret "update device username failed"

-- GET /api/devices/
getDeviceListHandler :: (HasPSQL u, HasOtherEnv Cache u, Monoid w) => ActionH u w ()
getDeviceListHandler = resultDeviceList getDevIdList countDevice

-- GET /api/users/:username/devices/
getDeviceListByNameHandler :: (HasPSQL u, HasOtherEnv Cache u, Monoid w) => ActionH u w ()
getDeviceListByNameHandler = do
  un <- captureParam "username"
  resultDeviceList (getDevIdListByName un) (countDeviceByName un)

-- DELETE /api/devices/:uuidOrToken/
-- DELETE /api/users/:username/devices/:uuidOrToken/
removeDeviceHandler :: (HasPSQL u, HasOtherEnv Cache u) => MqttEnv -> Device -> ActionH u w ()
removeDeviceHandler mqtt_ Device{devID = did, devUUID = uuid, devUserName = un} = do
  void $ lift $ removeDevice did
  liftIO $ sendDrop mqtt uuid
  resultOK

  where mqtt = if un `elem` mAllowKeys mqtt_ then mqtt_ {mKey = un} else mqtt_


-- GET /api/devices/:uuidOrToken/
-- GET /api/users/:username/devices/:uuidOrToken/
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
rpcHandler mqtt_ Device{devUUID = uuid, devUserName = un} = do
  payload <- formParam "payload"
  tout <- min 300 <$> safeFormParam "timeout" 300
  cacheHash <- safeFormParam "cache-hash" ""
  cacheTimeout <- safeFormParam "cache-timeout" 10
  let ca = if T.null cacheHash then id else cacheAble mqtt cacheHash cacheTimeout
  mmKey <- getKey
  r <- liftIO $ ca $ request mqtt mmKey uuid payload tout
  case r of
    Nothing -> err status500 "request timeout"
    Just v  -> do
      isjson <- safeFormParam "format" ("raw" :: String)
      when (isjson == "json") $ addHeader "Content-Type" "application/json; charset=utf-8"
      raw v

  where mqtt = if un `elem` mAllowKeys mqtt_ then mqtt_ {mKey = un} else mqtt_
