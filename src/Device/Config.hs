{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.Config
  ( PSQLConfig (..)
  , MqttConfig (..)
  , Config (..)
  , genPSQLPool
  , genRedisConnection
  , RedisConfig (..)
  , Cache
  , mkCache
  , redisEnv
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))
import           Database.Redis            (Connection)
import           Yuntan.Config.PSQLConfig  (PSQLConfig (..), genPSQLPool)
import           Yuntan.Config.RedisConfig (RedisConfig (..),
                                            defaultRedisConfig,
                                            genRedisConnection)
import           Yuntan.Types.HasPSQL      (HasOtherEnv, otherEnv)

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
  { psqlConfig  :: PSQLConfig
  , mqttConfig  :: MqttConfig
  , redisConfig :: RedisConfig
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    psqlConfig <- o .: "psql"
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
