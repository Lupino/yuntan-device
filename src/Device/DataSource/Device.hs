{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Device
  ( createDevice
  , getDevice
  , getDevIdByToken
  , getDevIdByUuid
  , getDevIdList
  , getDevIdListByKey
  , countDevice
  , countDeviceByKey
  , updateDevice
  , removeDevice

  , getDevKeyId
  , getDevKeyById

  , getDevIdByAddr
  , getDevIdListByGw
  , countDevAddrByGw
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe)
import           Data.String            (fromString)
import           Data.Text              (Text)
import           Data.UnixTime
import           Data.UUID              (toText)
import           Data.UUID.V4           (nextRandom)
import           Database.PSQL.Types    (From, Only (..), OrderBy, PSQL, Size,
                                         TableName, count, count_, delete,
                                         insertRet, selectOne, selectOneOnly,
                                         selectOnly, selectOnly_, update)
import           Device.Types

devices :: TableName
devices = "devices"

deviceKeys :: TableName
deviceKeys = "device_keys"


getDevKeyId_ :: Key -> PSQL (Maybe KeyID)
getDevKeyId_ key = selectOneOnly deviceKeys "id" "devkey = ?" (Only key)

createDevKey :: Key -> PSQL KeyID
createDevKey key = do
  t <- liftIO getUnixTime
  insertRet devices ["devkey", "created_at"] "id" (key, show $ toEpochTime t) 0

getDevKeyId :: Key -> PSQL KeyID
getDevKeyId key = do
  mkid <- getDevKeyId_ key
  case mkid of
    Nothing  -> createDevKey key
    Just kid -> pure kid

getDevKeyById :: KeyID -> PSQL Key
getDevKeyById kid = fromMaybe "" <$> selectOneOnly deviceKeys "devkey" "id = ?" (Only kid)

createDevice :: KeyID -> Token -> Addr -> PSQL DeviceID
createDevice kid token addr = do
  t <- liftIO getUnixTime
  uuid <- liftIO $ toText <$> nextRandom
  insertRet devices ["key_id", "token", "uuid", "addr", "gw_id", "meta", "created_at"] "id"
    (kid, token, uuid, addr, gwid, meta, show $ toEpochTime t) 0

  where meta :: String
        meta = "{}"

        gwid :: Int
        gwid = 0


getDevice :: DeviceID -> PSQL (Maybe Device)
getDevice devid = selectOne devices ["*"] "id = ?" (Only devid)

getDevIdByToken :: Token -> PSQL (Maybe DeviceID)
getDevIdByToken token = selectOneOnly devices "id" "token = ?" (Only token)

getDevIdByUuid :: UUID -> PSQL (Maybe DeviceID)
getDevIdByUuid uuid = selectOneOnly devices "id" "uuid = ?" (Only uuid)

getDevIdList :: From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdList = selectOnly_ devices "id"

countDevice :: PSQL Int64
countDevice = count_ devices

getDevIdListByKey :: KeyID -> From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdListByKey kid =
  selectOnly devices "id" "key_id = ?" (Only kid)

countDeviceByKey :: KeyID -> PSQL Int64
countDeviceByKey kid = count devices "key_id = ?" (Only kid)

updateDevice :: DeviceID -> String -> Text -> PSQL Int64
updateDevice devid field value =
  update devices [fromString field] "id = ?" (value, devid)

removeDevice :: DeviceID -> PSQL Int64
removeDevice devid = delete devices "id = ?" (Only devid)

getDevIdByAddr :: Addr -> PSQL (Maybe DeviceID)
getDevIdByAddr addr = selectOneOnly devices "id" "addr = ?" (Only addr)

getDevIdListByGw :: DeviceID -> From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdListByGw gwid =
  selectOnly devices "id" "gw_id = ?" (Only gwid)

countDevAddrByGw :: DeviceID -> PSQL Int64
countDevAddrByGw gwid = count devices "gw_id = ?" (Only gwid)
