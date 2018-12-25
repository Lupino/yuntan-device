{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.Config
  ( MySQLConfig (..)
  , MqttConfig (..)
  , Config (..)
  , genMySQLPool
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.:))

import           Data.Text                 (Text)
import           Network                   (HostName)
import           Yuntan.Config.MySQLConfig (MySQLConfig (..), genMySQLPool)

data MqttConfig = MqttConfig
  { mqttUsername :: Text
  , mqttPassword :: Text
  , mqttHost     :: HostName
  , mqttPort     :: Int
  } deriving (Show)

instance FromJSON MqttConfig where
  parseJSON = withObject "MqttConfig" $ \o -> do
    mqttUsername <- o .: "username"
    mqttPassword <- o .: "password"
    mqttHost <- o .: "host"
    mqttPort <- o .: "port"
    return MqttConfig{..}

data Config = Config
  { mysqlConfig :: MySQLConfig
  , mqttConfig  :: MqttConfig
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    mysqlConfig <- o .: "mysql"
    mqttConfig <- o .: "mqtt"
    return Config{..}
