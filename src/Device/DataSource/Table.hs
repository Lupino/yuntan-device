{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Table
  ( createTable
  ) where

import           Data.Int             (Int64)
import           Yuntan.Types.HasPSQL (PSQL, createIndex)
import qualified Yuntan.Types.HasPSQL as PSQL (createTable)

createDeviceTable :: PSQL Int64
createDeviceTable =
  PSQL.createTable "devices"
    [ "id SERIAL PRIMARY KEY"
    , "username VARCHAR(128) NOT NULL"
    , "token VARCHAR(128) NOT NULL"
    , "uuid VARCHAR(128) NOT NULL"
    , "meta JSON NOT NULL"
    , "type VARCHAR(128) NOT NULL"
    , "created_at INT NOT NULL"
    ]


createTable :: PSQL Int64
createTable prefix conn =
  sum <$> mapM (\o -> o prefix conn)
    [ createDeviceTable
    , createIndex True "devices" "device_token" ["token"]
    , createIndex True "devices" "device_uuid" ["uuid"]
    , createIndex False "devices" "device_username_type" ["username", "type"]
    , createIndex False "devices" "device_type" ["type"]
    ]
