{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Device
  ( createDevice
  , getDevice
  , getDeviceList

  , getDevKeyId
  , getDevKeyById
  , getDevKeyList

  , devices
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromMaybe)
import           Data.UUID              (toText)
import           Data.UUID.V4           (nextRandom)
import           Database.PSQL.Types    (Only (..), PSQL, TableName, insertRet,
                                         selectIn, selectOne, selectOneOnly)
import           Device.Types
import           Device.Util            (getEpochTime)

devices :: TableName
devices = "devices"

deviceKeys :: TableName
deviceKeys = "device_keys"


getDevKeyId_ :: Key -> PSQL (Maybe KeyID)
getDevKeyId_ key = selectOneOnly deviceKeys "id" "devkey = ?" (Only key)

createDevKey :: Key -> PSQL KeyID
createDevKey key = do
  t <- getEpochTime
  insertRet deviceKeys ["devkey", "created_at"] "id" (key, t) 0

getDevKeyId :: Key -> PSQL KeyID
getDevKeyId key = do
  mkid <- getDevKeyId_ key
  case mkid of
    Nothing  -> createDevKey key
    Just kid -> pure kid

getDevKeyById :: KeyID -> PSQL Key
getDevKeyById kid = fromMaybe "" <$> selectOneOnly deviceKeys "devkey" "id = ?" (Only kid)

getDevKeyList :: [KeyID] -> PSQL [(KeyID, Key)]
getDevKeyList = selectIn deviceKeys ["id", "devkey"] "id"

createDevice :: KeyID -> Token -> Addr -> PSQL DeviceID
createDevice kid token addr = do
  t <- getEpochTime
  uuid <- liftIO $ toText <$> nextRandom
  insertRet devices ["key_id", "token", "uuid", "addr", "gw_id", "meta", "created_at"] "id"
    (kid, token, uuid, addr, gwid, meta, t) 0

  where meta :: String
        meta = "{}"

        gwid :: Int
        gwid = 0


getDevice :: DeviceID -> PSQL (Maybe Device)
getDevice devid = selectOne devices ["*"] "id = ?" (Only devid)

getDeviceList :: [DeviceID] -> PSQL [Device]
getDeviceList = selectIn devices ["*"] "id"
