{-# LANGUAGE OverloadedStrings #-}
module Device.Handler
  ( requireDevice
  , requireOwner
  , createDeviceHandler
  , updateDeviceMetaHandler
  , updateDeviceTypeHandler
  , updateDeviceTokenHandler
  , getDeviceListHandler
  , getDeviceListByNameHandler
  , removeDeviceHandler
  , getDeviceHandler
  , rpcHandler
  ) where

import           Control.Monad           (void, when)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (lift)
import           Data.Aeson              (decode)
import           Data.ByteString.Lazy    (fromStrict)
import           Data.Int                (Int64)
import           Data.Maybe              (catMaybes)
import qualified Data.Text               as T (null, unpack)
import           Data.UUID               (fromText)
import           Device
import           Device.MQTT             (MqttEnv, request)
import           Haxl.Core               (GenHaxl)
import           Network.HTTP.Types      (status403, status500)
import           Network.MQTT            (Config)
import           Web.Scotty.Trans        (addHeader, json, param, raw)
import           Yuntan.Types.HasMySQL   (HasMySQL)
import           Yuntan.Types.ListResult (From, ListResult (..), Size)
import           Yuntan.Types.OrderBy    (OrderBy, desc)
import           Yuntan.Types.Scotty     (ActionH)
import           Yuntan.Utils.JSON       (unionValue)
import           Yuntan.Utils.Scotty     (err, errBadRequest, errNotFound, ok,
                                          okListResult, safeParam)

-- :uuidOrToken
apiDevice :: HasMySQL u => ActionH u (Maybe Device)
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

requireDevice :: HasMySQL u => (Device -> ActionH u ()) -> ActionH u ()
requireDevice next = do
  device <- apiDevice
  case device of
    Just o  -> next o
    Nothing -> errNotFound "Device is not found"

requireOwner :: (Device -> ActionH u ()) -> Device -> ActionH u ()
requireOwner next device = do
  username <- param "username"
  if devUserName device == username then next device
                                    else err status403 "no permission"

-- POST /api/devices/
-- POST /api/users/:username/devices/
createDeviceHandler :: HasMySQL u => ActionH u ()
createDeviceHandler = do
  username <- param "username"
  token <- param "token"
  tp <- param "type"
  devid <- lift $ createDevice username token tp
  json =<< lift (getDevice devid)

-- POST /api/devices/:uuidOrToken/token/
-- POST /api/users/:username/devices/:uuidOrToken/token/
updateDeviceTokenHandler :: HasMySQL u => Device -> ActionH u ()
updateDeviceTokenHandler Device{devID = did} = do
  token <- param "token"
  ret <- lift $ updateDeviceToken did token
  resultOKOrErr ret "update device token failed"

-- POST /api/devices/:uuidOrToken/type/
-- POST /api/users/:username/devices/:uuidOrToken/type/
updateDeviceTypeHandler :: HasMySQL u => Device -> ActionH u ()
updateDeviceTypeHandler Device{devID = did} = do
  tp <- param "type"
  ret <- lift $ updateDeviceType did tp
  resultOKOrErr ret "update device type failed"

-- POST /api/devices/:uuidOrToken/meta/
-- POST /api/users/:username/devices/:uuidOrToken/meta/
updateDeviceMetaHandler :: HasMySQL u => Device -> ActionH u ()
updateDeviceMetaHandler Device{devID = did, devMeta = ometa} = do
  meta <- param "meta"
  case decode meta of
    Just ev -> void (lift $ updateDeviceMeta did $ unionValue ev ometa) >> resultOK
    Nothing -> errBadRequest "meta filed is required."

-- GET /api/devices/
getDeviceListHandler :: HasMySQL u => ActionH u ()
getDeviceListHandler = do
  tp <- safeParam "type" ""
  if T.null tp then resultDeviceList getDevIdList countDevice
               else resultDeviceList (getDevIdListByType tp) (countDeviceByType tp)

-- GET /api/users/:username/devices/
getDeviceListByNameHandler :: HasMySQL u => ActionH u ()
getDeviceListByNameHandler = do
  un <- param "username"
  tp <- safeParam "type" ""
  if T.null tp then
    resultDeviceList (getDevIdListByName un) (countDeviceByName un)
  else
    resultDeviceList (getDevIdListByNameAndType un tp) (countDeviceByNameAndType un tp)

-- DELETE /api/devices/:uuidOrToken/
-- DELETE /api/users/:username/devices/:uuidOrToken/
removeDeviceHandler :: HasMySQL u => Device -> ActionH u ()
removeDeviceHandler Device{devID = did} = do
  void $ lift $ removeDevice did
  resultOK

-- GET /api/devices/:uuidOrToken/
-- GET /api/users/:username/devices/:uuidOrToken/
getDeviceHandler :: HasMySQL u => Device -> ActionH u ()
getDeviceHandler = ok "device"

resultOK :: ActionH u ()
resultOK = ok "result" ("OK" :: String)

resultOKOrErr :: Int64 -> String -> ActionH u ()
resultOKOrErr o m = if o > 0 then resultOK
                             else err status500 m

resultDeviceList :: HasMySQL u => (From -> Size -> OrderBy -> GenHaxl u [DeviceID]) -> GenHaxl u Int64 -> ActionH u ()
resultDeviceList getList count = do
  from <- safeParam "from" 0
  size <- safeParam "size" 10
  total <- lift count
  devices <- lift $ mapM getDevice =<< getList from size (desc "id")

  okListResult "devices" ListResult
    { getFrom   = from
    , getSize   = size
    , getTotal  = total
    , getResult = catMaybes devices
    }

rpcHandler :: HasMySQL u => MqttEnv -> Device -> ActionH u ()
rpcHandler mqtt Device{devUUID = uuid} = do
  key <- lift getPrefix
  payload <- param "payload"
  tout <- min 300 <$> safeParam "timeout" 300
  r <- liftIO $ request mqtt key (T.unpack uuid) payload tout
  case r of
    Nothing -> err status500 "request timeout"
    Just v  -> do
      isjson <- safeParam "format" ("raw" :: String)
      when (isjson == "json") $ addHeader "Content-Type" "application/json; charset=utf-8"
      raw $ fromStrict v
