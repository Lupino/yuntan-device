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
  , EmqxAuthConfig (..)
  , EmqxAdminConfig (..)
  ) where

import           Data.Aeson           (FromJSON, parseJSON, withObject, (.!=),
                                       (.:), (.:?))
import           Data.ByteString      (ByteString)
import           Data.String          (fromString)
import           Database.PSQL.Config (PSQL (..), genPSQLPool)
import           Database.PSQL.Types  (HasOtherEnv, otherEnv)
import           Database.Redis       (Connection)
import           Device.Types         (Key)
import           Haxl.RedisConfig     (RedisConfig (..), defaultRedisConfig,
                                       genRedisConnection)
import           Network.URI          (URI, parseURI)

data EmqxAdminConfig = EmqxAdminConfig
  { emqxAdminKey      :: String
  , emqxAdminPassword :: String
  }
  deriving (Show)

instance FromJSON EmqxAdminConfig where
  parseJSON = withObject "EmqxAdminConfig" $ \o -> do
    emqxAdminKey <- o .: "key"
    emqxAdminPassword <- o .: "password"
    return EmqxAdminConfig{ .. }

data EmqxAuthConfig = EmqxAuthConfig
  { emqxSuperAdmin    :: String
  , emqxSuperPassword :: String
  , emqxAdminList     :: [EmqxAdminConfig]
  }
  deriving (Show)

instance FromJSON EmqxAuthConfig where
  parseJSON = withObject "EmqxAuthConfig" $ \o -> do
    emqxSuperAdmin <- o .: "superadmin"
    emqxSuperPassword <- o .: "password"
    emqxAdminList <- o .: "admin_list"
    return EmqxAuthConfig{ .. }

data Config = Config
  { psqlConfig  :: PSQL
  , mqttConfig  :: URI
  , redisConfig :: RedisConfig
  , allowKeys   :: [Key]
  , emqxAuth    :: Maybe EmqxAuthConfig
  , authEnable  :: Bool
  , authKey     :: ByteString
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    psqlConfig <- o .: "psql"
    mqtt <- o .: "mqtt"
    redisConfig  <- o .:? "redis" .!= defaultRedisConfig
    allowKeys  <- o .:? "allow_keys" .!= []
    emqxAuth <- o .:? "emqx_auth"
    authEnable <- o .:? "auth_enable" .!= False
    authKey <- fromString <$> o .:? "auth_key" .!= ""
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
