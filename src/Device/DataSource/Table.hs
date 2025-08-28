{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Table
  ( createTable
  ) where

import           Data.Int      (Int64)
import           Database.PSQL (PSQL, createIndex)
import qualified Database.PSQL as PSQL (createTable)

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


createMetricTable :: PSQL Int64
createMetricTable =
  PSQL.createTable "metrics"
    [ "id BIGSERIAL PRIMARY KEY"
    , "dev_id INT NOT NULL"
    , "param VARCHAR(128) NOT NULL"
    , "raw_value VARCHAR(128) NOT NULL"
    , "value real NOT NULL"
    , "created_at INT NOT NULL"
    ]


createIndexNameTable :: PSQL Int64
createIndexNameTable =
  PSQL.createTable "index_names"
    [ "id SERIAL PRIMARY KEY"
    , "name VARCHAR(128) NOT NULL"
    , "created_at INT NOT NULL"
    ]


createIndexTable :: PSQL Int64
createIndexTable =
  PSQL.createTable "indexs"
    [ "id SERIAL PRIMARY KEY"
    , "name_id INT NOT NULL"
    , "dev_id INT NOT NULL"
    , "created_at INT NOT NULL"
    ]


createCardTable :: PSQL Int64
createCardTable =
  PSQL.createTable "cards"
    [ "id SERIAL PRIMARY KEY"
    , "dev_id INT NOT NULL"
    , "param VARCHAR(128) NOT NULL"
    , "meta JSON NOT NULL"
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
    , createMetricTable
    , createIndex True "metrics" "metric_uniq_key" ["dev_id", "param", "created_at"]

    , createIndexNameTable
    , createIndex True "index_names" "index_name_uniq_key" ["name"]
    , createIndexTable
    , createIndex True "indexs" "index_uniq_key" ["name_id", "dev_id"]

    , createCardTable
    , createIndex True "cards" "card_uniq_key" ["dev_id", "param"]
    ]
