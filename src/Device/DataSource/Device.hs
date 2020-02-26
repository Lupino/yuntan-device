{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Device
  ( createDevice
  , getDevice
  , getDevIdByToken
  , getDevIdByUuid
  , getDevIdList
  , getDevIdListByName
  , getDevIdListByType
  , getDevIdListByNameAndType
  , countDevice
  , countDeviceByName
  , countDeviceByType
  , countDeviceByNameAndType
  , updateDevice
  , removeDevice
  ) where

import           Data.Int                   (Int64)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import           Data.UnixTime
import           Data.UUID                  (toText)
import           Data.UUID.V4               (nextRandom)
import           Database.PostgreSQL.Simple (Only (..))
import           Device.Types
import           Yuntan.Types.HasPSQL       (PSQL, TableName, count, count_,
                                             delete, insertRet, selectOne,
                                             selectOneOnly, selectOnly,
                                             selectOnly_, update)
import           Yuntan.Types.ListResult    (From, Size)
import           Yuntan.Types.OrderBy       (OrderBy)

devices :: TableName
devices = "devices"

createDevice :: UserName -> Token -> Type -> PSQL DeviceID
createDevice un token tp prefix conn = do
  t <- getUnixTime
  uuid <- toText <$> nextRandom
  insertRet devices ["username", "token", "uuid", "meta", "type", "created_at"] "id"
    (un, token, uuid, "{}" :: String, tp, show $ toEpochTime t) prefix conn

getDevice :: DeviceID -> PSQL (Maybe Device)
getDevice devid = selectOne devices ["*"] "id = ?" (Only devid)

getDevIdByToken :: Token -> PSQL (Maybe DeviceID)
getDevIdByToken token = selectOneOnly devices "id" "token = ?" (Only token)

getDevIdByUuid :: UUID -> PSQL (Maybe DeviceID)
getDevIdByUuid uuid = selectOneOnly devices "id" "uuid = ?" (Only uuid)

getDevIdList :: From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdList from size o = selectOnly_ devices "id" from size o

countDevice :: PSQL Int64
countDevice = count_ devices

getDevIdListByName :: UserName -> From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdListByName un from size o =
  selectOnly devices "id" "username = ?" (Only un) from size o

countDeviceByName :: UserName -> PSQL Int64
countDeviceByName un = count devices "username = ?" (Only un)

getDevIdListByType :: Type -> From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdListByType tp from size o =
  selectOnly devices "id" "type = ?" (Only tp) from size o

countDeviceByType :: Type -> PSQL Int64
countDeviceByType tp = count devices "type = ?" (Only tp)

getDevIdListByNameAndType :: UserName -> Type -> From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdListByNameAndType un tp from size o =
  selectOnly devices "id" "username = ? AND type = ?" (un, tp) from size o

countDeviceByNameAndType :: UserName -> Type -> PSQL Int64
countDeviceByNameAndType un tp =
  count devices "username = ? AND type = ?" (un, tp)

updateDevice :: DeviceID -> String -> Text -> PSQL Int64
updateDevice devid field value =
  update devices [fromString field] "id = ?" (value, devid)

removeDevice :: DeviceID -> PSQL Int64
removeDevice devid = delete devices "id = ?" (Only devid)
