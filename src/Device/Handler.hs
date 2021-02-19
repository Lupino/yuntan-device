{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Device.Handler
  ( requireDevice
  , requireOwner
  , createDeviceHandler
  , updateDeviceMetaHandler
  , updateDeviceTypeHandler
  , updateDeviceTokenHandler
  , updateDeviceUserNameHandler
  , getDeviceListHandler
  , getDeviceListByNameHandler
  , removeDeviceHandler
  , getDeviceHandler
  , rpcHandler
  ) where

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (lift)
import           Data.Aeson             (decode)
import           Data.Aeson.Helper      (union)
import           Data.Aeson.Result      (List (..))
import           Data.Int               (Int64)
import           Data.Maybe             (catMaybes)
import qualified Data.Text              as T (null)
import           Data.UUID              (fromText)
import           Database.PSQL.Types    (From (..), HasOtherEnv, HasPSQL,
                                         OrderBy, Size (..), desc)
import           Device
import           Device.Config          (Cache)
import           Device.MQTT            (MqttEnv (mAllowKeys, mKey), cacheAble,
                                         request)
import           Haxl.Core              (GenHaxl)
import           Network.HTTP.Types     (status403, status500)
import           Web.Scotty.Haxl        (ActionH)
import           Web.Scotty.Trans       (addHeader, json, param, raw)
import           Web.Scotty.Utils       (err, errBadRequest, errNotFound, ok,
                                         okListResult, safeParam)

-- :uuidOrToken
apiDevice :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w (Maybe Device)
apiDevice = do
  uuidOrToken <- param "uuidOrToken"
  let f = case fromText uuidOrToken of
            Just _  -> getDevIdByUuid
            Nothing -> getDevIdByToken

  lift $ do
    devid <- f uuidOrToken
    case devid of
      Nothing  -> pure Nothing
      Just did -> getDevice did

requireDevice :: (HasPSQL u, HasOtherEnv Cache u) => (Device -> ActionH u w ()) -> ActionH u w ()
requireDevice next = do
  device <- apiDevice
  case device of
    Just o  -> next o
    Nothing -> errNotFound "Device is not found"

requireOwner :: (Device -> ActionH u w ()) -> Device -> ActionH u w ()
requireOwner next device = do
  username <- param "username"
  if devUserName device == username then next device
                                    else err status403 "no permission"

-- POST /api/devices/
-- POST /api/users/:username/devices/
createDeviceHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
createDeviceHandler = do
  username <- param "username"
  token <- param "token"
  tp <- param "type"
  olddevid <- lift $ getDevIdByToken token
  case olddevid of
    Just _ -> errBadRequest "token is already used."
    Nothing -> do
      devid <- lift $ createDevice username token tp
      json =<< lift (getDevice devid)

-- POST /api/devices/:uuidOrToken/token/
-- POST /api/users/:username/devices/:uuidOrToken/token/
updateDeviceTokenHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDeviceTokenHandler Device{devID = did} = do
  token <- param "token"
  ret <- lift $ updateDeviceToken did token
  resultOKOrErr ret "update device token failed"

-- POST /api/devices/:uuidOrToken/type/
-- POST /api/users/:username/devices/:uuidOrToken/type/
updateDeviceTypeHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDeviceTypeHandler Device{devID = did} = do
  tp <- param "type"
  ret <- lift $ updateDeviceType did tp
  resultOKOrErr ret "update device type failed"

-- POST /api/devices/:uuidOrToken/meta/
-- POST /api/users/:username/devices/:uuidOrToken/meta/
updateDeviceMetaHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDeviceMetaHandler Device{devID = did, devMeta = ometa} = do
  meta <- param "meta"
  case decode meta of
    Just ev -> void (lift $ updateDeviceMeta did $ union ev ometa) >> resultOK
    Nothing -> errBadRequest "meta filed is required."

-- POST /api/devices/:uuidOrToken/username/
updateDeviceUserNameHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
updateDeviceUserNameHandler Device{devID = did} = do
  un <- param "username"
  ret <- lift $ updateDeviceUserName did un
  resultOKOrErr ret "update device username failed"

-- GET /api/devices/
getDeviceListHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getDeviceListHandler = do
  tp <- safeParam "type" ""
  if T.null tp then resultDeviceList getDevIdList countDevice
               else resultDeviceList (getDevIdListByType tp) (countDeviceByType tp)

-- GET /api/users/:username/devices/
getDeviceListByNameHandler :: (HasPSQL u, HasOtherEnv Cache u) => ActionH u w ()
getDeviceListByNameHandler = do
  un <- param "username"
  tp <- safeParam "type" ""
  if T.null tp then
    resultDeviceList (getDevIdListByName un) (countDeviceByName un)
  else
    resultDeviceList (getDevIdListByNameAndType un tp) (countDeviceByNameAndType un tp)

-- DELETE /api/devices/:uuidOrToken/
-- DELETE /api/users/:username/devices/:uuidOrToken/
removeDeviceHandler :: (HasPSQL u, HasOtherEnv Cache u) => Device -> ActionH u w ()
removeDeviceHandler Device{devID = did} = do
  void $ lift $ removeDevice did
  resultOK

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
  :: (HasPSQL u, HasOtherEnv Cache u)
  => (From -> Size -> OrderBy -> GenHaxl u w [DeviceID])
  -> GenHaxl u w Int64 -> ActionH u w ()
resultDeviceList getList count = do
  from <- From <$> safeParam "from" 0
  size <- Size <$> safeParam "size" 10
  total <- lift count
  devices <- lift $ mapM getDevice =<< getList from size (desc "id")

  okListResult "devices" List
    { getFrom   = unFrom from
    , getSize   = unSize size
    , getTotal  = total
    , getResult = catMaybes devices
    }

rpcHandler :: HasPSQL u => MqttEnv -> Device -> ActionH u w ()
rpcHandler mqtt_ Device{devUUID = uuid, devUserName = un} = do
  payload <- param "payload"
  tout <- min 300 <$> safeParam "timeout" 300
  cacheHash <- safeParam "cache-hash" ""
  cacheTimeout <- safeParam "cache-timeout" 10
  let ca = if T.null cacheHash then id else cacheAble mqtt cacheHash cacheTimeout
  r <- liftIO $ ca $ request mqtt uuid payload tout
  case r of
    Nothing -> err status500 "request timeout"
    Just v  -> do
      isjson <- safeParam "format" ("raw" :: String)
      when (isjson == "json") $ addHeader "Content-Type" "application/json; charset=utf-8"
      raw v

  where mqtt = if un `elem` mAllowKeys mqtt_ then mqtt_ {mKey = un} else mqtt_
