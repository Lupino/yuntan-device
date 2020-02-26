{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Table
  ( createTable
  ) where

import           Data.Int             (Int64)
import           Yuntan.Types.HasPSQL (PSQL, createIndex)
import qualified Yuntan.Types.HasPSQL as PSQL (createTable)

createDeviceTable :: PSQL Int64
createDeviceTable prefix conn =
  PSQL.createTable "devices"
    [ "id SERIAL PRIMARY KEY"
    , "username VARCHAR(20) NOT NULL"
    , "token VARCHAR(36) NOT NULL"
    , "uuid VARCHAR(36) NOT NULL"
    , "meta JSON NOT NULL"
    , "type VARCHAR(10) NOT NULL"
    , "created_at INT NOT NULL"
    ] prefix conn


createTable :: PSQL Int64
createTable prefix conn =
  sum <$> mapM (\o -> o prefix conn)
    [ createDeviceTable
    , createIndex True "devices" "device_token" ["token"]
    , createIndex True "devices" "device_uuid" ["uuid"]
    , createIndex False "devices" "device_username_type" ["username", "type"]
    , createIndex False "devices" "device_type" ["type"]
    ]
