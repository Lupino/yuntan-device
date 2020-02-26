{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Table
  ( createTable
  ) where

import           Data.Int                   (Int64)
import           Data.String                (fromString)
import           Database.PostgreSQL.Simple (execute_)
import           Yuntan.Types.HasPSQL       (PSQL)

createDeviceTable :: PSQL Int64
createDeviceTable prefix conn = execute_ conn sql
  where sql = fromString $ concat
          [ "CREATE TABLE IF NOT EXISTS ", prefix, "_devices ("
          , "  id SERIAL PRIMARY KEY,"
          , "  username CHAR(20) NOT NULL,"
          , "  token CHAR(36) NOT NULL,"
          , "  uuid CHAR(36) NOT NULL,"
          , "  meta JSON NOT NULL,"
          , "  type CHAR(10) NOT NULL,"
          , "  created_at INT NOT NULL"
          , ")"
          ]

createIndex :: Bool -> String -> String -> String -> PSQL Int64
createIndex uniq tableName indexName columns prefix conn = execute_ conn sql
  where sql = fromString $ concat
          [ "CREATE ", uniqWord, "INDEX IF NOT EXISTS ", indexName
          , " ON " , prefix, tableName, "(", columns, ")"
          ]

        uniqWord = if uniq then "UNIQUE " else ""


createTable :: PSQL Int64
createTable prefix conn =
  sum <$> mapM (\o -> o prefix conn)
    [ createDeviceTable
    , createIndex True "_devices" "device_token" "token"
    , createIndex True "_devices" "device_uuid" "uuid"
    , createIndex False "_devices" "device_username_type" "username, type"
    , createIndex False "_devices" "device_type" "type"
    ]
