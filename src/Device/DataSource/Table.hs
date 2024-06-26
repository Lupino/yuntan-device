{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Table
  ( createTable
  ) where

import           Data.Int            (Int64)
import           Database.PSQL.Types (PSQL, createIndex)
import qualified Database.PSQL.Types as PSQL (createTable)

createDeviceTable :: PSQL Int64
createDeviceTable =
  PSQL.createTable "devices"
    [ "id SERIAL PRIMARY KEY"
    , "key_id INT NOT NULL"
    , "token VARCHAR(128) NOT NULL"
    , "uuid VARCHAR(128) NOT NULL"
    , "addr VARCHAR(20) NOT NULL"
    , "gw_id INT NOT NULL"
    , "meta JSON NOT NULL"
    , "created_at INT NOT NULL"
    ]

createDeviceKeyTable :: PSQL Int64
createDeviceKeyTable =
  PSQL.createTable "device_keys"
    [ "id SERIAL PRIMARY KEY"
    , "devkey VARCHAR(128) NOT NULL"
    , "created_at INT NOT NULL"
    ]


createTable :: PSQL Int64
createTable =
  sum <$> sequence
    [ createDeviceTable
    , createIndex True "devices" "device_token" ["token"]
    , createIndex True "devices" "device_uuid" ["uuid"]
    , createIndex False "devices" "device_key_id" ["key_id"]
    , createIndex True "devices" "device_addr" ["addr"]
    , createIndex False "devices" "device_gw_id" ["gw_id"]
    , createDeviceKeyTable
    , createIndex True "device_keys" "device_key_devkey" ["devkey"]
    ]
