{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.Config
  ( PSQL (..)
  , Config (..)
  , genPSQLPool
  , genRedisConnection
  , RedisConfig (..)
  , Cache
  , mkCache
  , redisEnv
  ) where

import           Data.Aeson           (FromJSON, parseJSON, withObject, (.!=),
                                       (.:), (.:?))
import           Database.PSQL.Config (PSQL (..), genPSQLPool)
import           Database.PSQL.Types  (HasOtherEnv, otherEnv)
import           Database.Redis       (Connection)
import           Device.Types         (Key)
import           Haxl.RedisConfig     (RedisConfig (..), defaultRedisConfig,
                                       genRedisConnection)
import           Network.URI          (URI, parseURI)

data Config = Config
    { psqlConfig  :: PSQL
    , mqttConfig  :: URI
    , redisConfig :: RedisConfig
    , allowKeys   :: [Key]
    }
    deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    psqlConfig <- o .: "psql"
    mqtt <- o .: "mqtt"
    redisConfig  <- o .:? "redis" .!= defaultRedisConfig
    allowKeys  <- o .:? "allow_keys" .!= []
    case parseURI mqtt of
      Nothing         -> fail "invalid mqtt uri"
      Just mqttConfig -> return Config{..}

newtype Cache = Cache
  { redis :: Maybe Connection
  }

redisEnv :: (HasOtherEnv Cache u) => u -> Maybe Connection
redisEnv = redis . otherEnv

mkCache :: Maybe Connection -> Cache
mkCache = Cache
