{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Device
  ( getDevice
  , getDeviceList

  , getDevKeyById
  , getDevKeyList

  , devices
  , deviceKeys
  ) where

import           Database.PSQL (Only (..), PSQL, TableName, selectIn, selectOne,
                                selectOneOnly)
import           Device.Types

devices :: TableName
devices = "devices"

deviceKeys :: TableName
deviceKeys = "device_keys"

getDevKeyById :: KeyID -> PSQL (Maybe Key)
getDevKeyById kid = selectOneOnly deviceKeys "devkey" "id = ?" (Only kid)

getDevKeyList :: [KeyID] -> PSQL [(KeyID, Key)]
getDevKeyList = selectIn deviceKeys ["id", "devkey"] "id"

getDevice :: DeviceID -> PSQL (Maybe Device)
getDevice devid = selectOne devices ["*"] "id = ?" (Only devid)

getDeviceList :: [DeviceID] -> PSQL [Device]
getDeviceList = selectIn devices ["*"] "id"
