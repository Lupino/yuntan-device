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
  , updateDeviceMeta
  , updateDeviceToken
  , updateDeviceType
  , removeDevice
  ) where

import           Control.Monad           (void)
import           Data.Aeson              (encode)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.String             (fromString)
import           Data.UnixTime
import           Data.UUID               (toText)
import           Data.UUID.V4            (nextRandom)
import           Database.MySQL.Simple   (Only (..), execute, insertID, query,
                                          query_)
import           Device.Types
import           Yuntan.Types.HasMySQL   (MySQL)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createDevice :: UserName -> Token -> Type -> MySQL DeviceID
createDevice un token tp prefix conn = do
  t <- getUnixTime
  uuid <- toText <$> nextRandom
  void $ execute conn insertSQL (un, token, uuid, "{}" :: String, tp, show $ toEpochTime t)
  fromIntegral <$> insertID conn

  where insertSQL = fromString $ concat
          [ "INSERT INTO `", prefix, "_devices` "
          , "(`username`, `token`, `uuid`, `meta`, `type`, `created_at`)"
          , " VALUES "
          , "(?, ?, ?, ?, ?, ?)"
          ]

getDevice :: DeviceID -> MySQL (Maybe Device)
getDevice devid prefix conn = listToMaybe <$> query conn sql (Only devid)
  where sql = fromString $ concat
          [ "SELECT * FROM `", prefix, "_devices` "
          , "WHERE `id` = ?"
          ]

getDevIdByToken :: Token -> MySQL (Maybe DeviceID)
getDevIdByToken token prefix conn =
  fmap fromOnly . listToMaybe <$> query conn sql (Only token)
  where sql = fromString $ concat
          [ "SELECT `id` FROM `", prefix, "_devices` WHERE `token`=?"
          ]

getDevIdByUuid :: UUID -> MySQL (Maybe DeviceID)
getDevIdByUuid uuid prefix conn =
  fmap fromOnly . listToMaybe <$> query conn sql (Only uuid)
  where sql = fromString $ concat
          [ "SELECT `id` FROM `", prefix, "_devices` WHERE `uuid`=?"
          ]

getDevIdList :: From -> Size -> OrderBy -> MySQL [DeviceID]
getDevIdList from size o prefix conn =
  map fromOnly <$> query conn sql (from, size)
  where sql = fromString $ concat
          [ "SELECT `id` FROM `", prefix, "_devices` ", show o, " LIMIT ?,?"
          ]

countDevice :: MySQL Int64
countDevice prefix conn = maybe 0 fromOnly . listToMaybe <$> query_ conn sql
  where sql = fromString $ concat
          [ "SELECT count(*) FROM `", prefix, "_devices`"
          ]

getDevIdListByName :: UserName -> From -> Size -> OrderBy -> MySQL [DeviceID]
getDevIdListByName un from size o prefix conn =
  map fromOnly <$> query conn sql (un, from, size)
  where sql = fromString $ concat
          [ "SELECT `id` FROM `", prefix, "_devices` WHERE `username` = ? ", show o, " LIMIT ?,?"
          ]

countDeviceByName :: UserName -> MySQL Int64
countDeviceByName un prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (Only un)
  where sql = fromString $ concat
          [ "SELECT count(*) FROM `", prefix, "_devices` WHERE `username` = ?"
          ]

getDevIdListByType :: Type -> From -> Size -> OrderBy -> MySQL [DeviceID]
getDevIdListByType tp from size o prefix conn =
  map fromOnly <$> query conn sql (tp, from, size)
  where sql = fromString $ concat
          [ "SELECT `id` FROM `", prefix, "_devices` WHERE `type` = ? ", show o, " LIMIT ?,?"
          ]

countDeviceByType :: Type -> MySQL Int64
countDeviceByType tp prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (Only tp)
  where sql = fromString $ concat
          [ "SELECT count(*) FROM `", prefix, "_devices` WHERE `type` = ?"
          ]

getDevIdListByNameAndType :: UserName -> Type -> From -> Size -> OrderBy -> MySQL [DeviceID]
getDevIdListByNameAndType un tp from size o prefix conn =
  map fromOnly <$> query conn sql (un, tp, from, size)
  where sql = fromString $ concat
          [ "SELECT `id` FROM `", prefix, "_devices` WHERE `username` = ? AND `type` = ? ", show o, " LIMIT ?,?"
          ]

countDeviceByNameAndType :: UserName -> Type -> MySQL Int64
countDeviceByNameAndType un tp prefix conn =
  maybe 0 fromOnly . listToMaybe <$> query conn sql (un, tp)
  where sql = fromString $ concat
          [ "SELECT count(*) FROM `", prefix, "_devices` WHERE `username` = ? AND `type` = ?"
          ]

updateDeviceMeta :: DeviceID -> Meta -> MySQL Int64
updateDeviceMeta devid meta prefix conn = execute conn sql (encode meta, devid)
  where sql = fromString $ concat
          [ "UPDATE `", prefix, "_devices` SET `meta` = ? WHERE `id`=?"
          ]

updateDeviceType :: DeviceID -> Type -> MySQL Int64
updateDeviceType devid tp prefix conn = execute conn sql (tp, devid)
  where sql = fromString $ concat
          [ "UPDATE `", prefix, "_devices` SET `type` = ? WHERE `id`=?"
          ]

updateDeviceToken :: DeviceID -> Token -> MySQL Int64
updateDeviceToken devid token prefix conn = execute conn sql (token, devid)
  where sql = fromString $ concat
          [ "UPDATE `", prefix, "_devices` SET `token` = ? WHERE `id`=?"
          ]

removeDevice :: DeviceID -> MySQL Int64
removeDevice devid prefix conn = execute conn sql (Only devid)
  where sql = fromString $ concat
          [ "DELETE FROM `", prefix, "_devices` WHERE `id`=?"
          ]
