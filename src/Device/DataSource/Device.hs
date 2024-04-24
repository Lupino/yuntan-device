{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Device
  ( createDevice
  , getDevice
  , getDevIdByToken
  , getDevIdByUuid
  , getDevIdList
  , getDevIdListByName
  , countDevice
  , countDeviceByName
  , updateDevice
  , removeDevice
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
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

createDevice :: UserName -> Token -> PSQL DeviceID
createDevice un token = do
  t <- liftIO getUnixTime
  uuid <- liftIO $ toText <$> nextRandom
  insertRet devices ["username", "token", "uuid", "meta", "created_at"] "id"
    (un, token, uuid, "{}" :: String, show $ toEpochTime t) 0

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

getDevIdListByName :: UserName -> From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdListByName un =
  selectOnly devices "id" "username = ?" (Only un)

countDeviceByName :: UserName -> PSQL Int64
countDeviceByName un = count devices "username = ?" (Only un)

updateDevice :: DeviceID -> String -> Text -> PSQL Int64
updateDevice devid field value =
  update devices [fromString field] "id = ?" (value, devid)

removeDevice :: DeviceID -> PSQL Int64
removeDevice devid = delete devices "id = ?" (Only devid)
