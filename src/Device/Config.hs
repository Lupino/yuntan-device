{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.Config
  ( MySQLConfig (..)
  , MqttConfig (..)
  , Config (..)
  , genMySQLPool
  , genRedisConnection
  , RedisConfig (..)
  , Cache
  , mkCache
  , redisEnv
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))
import           Data.Text                 (Text)
import           Database.Redis            (Connection)
import           Network.Socket            (HostName)
import           Yuntan.Config.MySQLConfig (MySQLConfig (..), genMySQLPool)
import           Yuntan.Config.RedisConfig (RedisConfig (..),
                                            defaultRedisConfig,
                                            genRedisConnection)
import           Yuntan.Types.HasMySQL     (HasOtherEnv, otherEnv)

data MqttConfig = MqttConfig
  { mqttUsername :: String
  , mqttPassword :: String
  , mqttHost     :: String
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
  , redisConfig :: RedisConfig
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    mysqlConfig <- o .: "mysql"
    mqttConfig <- o .: "mqtt"
    redisConfig  <- o .:? "redis" .!= defaultRedisConfig
    return Config{..}

newtype Cache = Cache
  { redis :: Maybe Connection
  }

redisEnv :: (HasOtherEnv Cache u) => u -> Maybe Connection
redisEnv = redis . otherEnv

mkCache :: Maybe Connection -> Cache
mkCache = Cache
