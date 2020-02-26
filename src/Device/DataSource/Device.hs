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
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import           Data.UnixTime
import           Data.UUID                  (toText)
import           Data.UUID.V4               (nextRandom)
import           Database.PostgreSQL.Simple (Only (..), execute, query, query_)
import           Device.Types
import           Yuntan.Types.HasPSQL       (PSQL)
import           Yuntan.Types.ListResult    (From, Size)
import           Yuntan.Types.OrderBy       (OrderBy, show1)

createDevice :: UserName -> Token -> Type -> PSQL DeviceID
createDevice un token tp prefix conn = do
  t <- getUnixTime
  uuid <- toText <$> nextRandom
  fromMaybe 0
    . fmap fromOnly
    . listToMaybe
    <$> query conn insertSQL (un, token, uuid, "{}" :: String, tp, show $ toEpochTime t)

  where insertSQL = fromString $ concat
          [ "INSERT INTO ", prefix, "_devices "
          , "(username, token, uuid, meta, type, created_at)"
          , " VALUES "
          , "(?, ?, ?, ?, ?, ?) returning id"
          ]

getDevice :: DeviceID -> PSQL (Maybe Device)
getDevice devid prefix conn = listToMaybe <$> query conn sql (Only devid)
  where sql = fromString $ concat
          [ "SELECT * FROM ", prefix, "_devices "
          , "WHERE id = ?"
          ]

getDevIdByToken :: Token -> PSQL (Maybe DeviceID)
getDevIdByToken token prefix conn =
  fmap fromOnly . listToMaybe <$> query conn sql (Only token)
  where sql = fromString $ concat
          [ "SELECT id FROM ", prefix, "_devices WHERE token=?"
          ]

getDevIdByUuid :: UUID -> PSQL (Maybe DeviceID)
getDevIdByUuid uuid prefix conn =
  fmap fromOnly . listToMaybe <$> query conn sql (Only uuid)
  where sql = fromString $ concat
          [ "SELECT id FROM ", prefix, "_devices WHERE uuid = ?"
          ]

getDevIdList :: From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdList from size o prefix conn = do
  print sql
  map fromOnly <$> query conn sql (size, from)
  where sql = fromString $ concat
          [ "SELECT id FROM ", prefix, "_devices ", show1 o, " LIMIT ? OFFSET ?"
          ]

countDevice :: PSQL Int64
countDevice prefix conn = maybe 0 fromOnly . listToMaybe <$> query_ conn sql
  where sql = fromString $ concat
          [ "SELECT count(*) FROM ", prefix, "_devices"
          ]

getDevIdListByName :: UserName -> From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdListByName un from size o prefix conn =
  map fromOnly <$> query conn sql (un, size, from)
  where sql = fromString $ concat
          [ "SELECT id FROM ", prefix, "_devices WHERE username = ? ", show1 o, " LIMIT ? OFFSET ?"
          ]

countDeviceByName :: UserName -> PSQL Int64
countDeviceByName un prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (Only un)
  where sql = fromString $ concat
          [ "SELECT count(*) FROM ", prefix, "_devices WHERE username = ?"
          ]

getDevIdListByType :: Type -> From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdListByType tp from size o prefix conn =
  map fromOnly <$> query conn sql (tp, size, from)
  where sql = fromString $ concat
          [ "SELECT id FROM ", prefix, "_devices WHERE type = ? ", show1 o, " LIMIT ? OFFSET ?"
          ]

countDeviceByType :: Type -> PSQL Int64
countDeviceByType tp prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (Only tp)
  where sql = fromString $ concat
          [ "SELECT count(*) FROM ", prefix, "_devices WHERE type = ?"
          ]

getDevIdListByNameAndType :: UserName -> Type -> From -> Size -> OrderBy -> PSQL [DeviceID]
getDevIdListByNameAndType un tp from size o prefix conn =
  map fromOnly <$> query conn sql (un, tp, size, from)
  where sql = fromString $ concat
          [ "SELECT id FROM ", prefix, "_devices WHERE username = ? AND type = ? ", show1 o, " LIMIT ? OFFSET ?"
          ]

countDeviceByNameAndType :: UserName -> Type -> PSQL Int64
countDeviceByNameAndType un tp prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (un, tp)
  where sql = fromString $ concat
          [ "SELECT count(*) FROM ", prefix, "_devices WHERE username = ? AND type = ?"
          ]

updateDevice :: DeviceID -> String -> Text -> PSQL Int64
updateDevice devid field value prefix conn = execute conn sql (value, devid)
  where sql = fromString $ concat
          [ "UPDATE ", prefix, "_devices SET ", field , " = ? WHERE id = ?"
          ]

removeDevice :: DeviceID -> PSQL Int64
removeDevice devid prefix conn = execute conn sql (Only devid)
  where sql = fromString $ concat
          [ "DELETE FROM ", prefix, "_devices WHERE id = ?"
          ]
