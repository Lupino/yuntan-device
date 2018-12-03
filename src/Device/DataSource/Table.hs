{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Table
  ( createTable
  ) where

import           Database.MySQL.Simple (Connection, execute_)

import           Data.Int              (Int64)
import           Data.String           (fromString)

import           Device.Types

createDeviceTable :: TablePrefix -> Connection -> IO Int64
createDeviceTable prefix conn = execute_ conn sql
  where sql = fromString $ concat
          [ "CREATE TABLE IF NOT EXISTS `", prefix, "_devices` ("
          , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
          , "  `username` varchar(128) NOT NULL,"
          , "  `token` varchar(128) NOT NULL,"
          , "  `uuid` varchar(128) NOT NULL,"
          , "  `meta` varchar(1500) NOT NULL,"
          , "  `type` varchar(128) NOT NULL,"
          , "  `created_at` int(10) unsigned NOT NULL,"
          , "  PRIMARY KEY (`id`),"
          , "  UNIQUE KEY `device_token` (`token`),"
          , "  UNIQUE KEY `device_uuid` (`uuid`),"
          , "  KEY `device_username_type` (`username`, `type`),"
          , "  KEY `device_type` (`type`)"
          , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
          ]


createTable :: TablePrefix -> Connection -> IO Int64
createTable prefix conn =
  sum <$> mapM (\o -> o prefix conn) [ createDeviceTable ]
