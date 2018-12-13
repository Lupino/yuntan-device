{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.Config
  ( MySQLConfig (..)
  , MQConfig (..)
  , Config (..)
  , genMySQLPool
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.:))

import           Yuntan.Config.MySQLConfig (MySQLConfig (..), genMySQLPool)

data MQConfig = MQConfig
  { mqttUsername :: String
  , mqttPassword :: String
  , mqttHost     :: String
  , mqttPort     :: Int
  } deriving (Show)

instance FromJSON MQConfig where
  parseJSON = withObject "MQConfig" $ \o -> do
    mqttUsername <- o .: "username"
    mqttPassword <- o .: "password"
    mqttHost <- o .: "host"
    mqttPort <- o .: "port"
    return MQConfig{..}

data Config = Config
  { mysqlConfig :: MySQLConfig
  , mqttConfig  :: MQConfig
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    mysqlConfig <- o .: "mysql"
    mqttConfig <- o .: "mqtt"
    return Config{..}
