{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (delete, get, middleware,
                                                       post, scottyOptsT,
                                                       settings)

import           Data.String                          (fromString)
import           Yuntan.Types.HasPSQL                 (HasOtherEnv, HasPSQL,
                                                       simpleEnv)
import           Yuntan.Types.Scotty                  (ScottyH)
import           Yuntan.Utils.RedisCache              (initRedisState)

import           Device
import           Device.Handler
import           Haxl.Core                            (GenHaxl, StateStore,
                                                       initEnv, runHaxl,
                                                       stateEmpty, stateSet)

import qualified Data.Yaml                            as Y
import qualified Device.Config                        as C
import           Device.MQTT                          (MqttEnv, startMQTT)

import           Data.Semigroup                       ((<>))
import           Options.Applicative

data Options = Options
  { getConfigFile  :: String
  , getHost        :: String
  , getPort        :: Int
  , getTablePrefix :: String
  }

parser :: Parser Options
parser = Options
  <$> strOption (long "config"
                 <> short 'c'
                 <> metavar "FILE"
                 <> help "Device micro server config file."
                 <> value "config.yaml")
  <*> strOption (long "host"
                 <> short 'H'
                 <> metavar "HOST"
                 <> help "Device micro server hostname."
                 <> value "127.0.0.1")
  <*> option auto (long "port"
                 <> short 'p'
                 <> metavar "PORT"
                 <> help "Device micro server port."
                 <> value 3000)
  <*> strOption (long "table_prefix"
                 <> metavar "TABLE_PREFIX"
                 <> help "table prefix."
                 <> value "test")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Device micro server"
     <> header "yuntan-cart - Device micro server" )

program :: Options -> IO ()
program Options { getConfigFile  = confFile
                , getTablePrefix = prefix
                , getHost        = host
                , getPort        = port
                } = do
  (Right conf) <- Y.decodeFileEither confFile

  let psqlConfig  = C.psqlConfig conf
      psqlThreads = C.psqlHaxlNumThreads psqlConfig
      redisConfig  = C.redisConfig conf
      redisThreads = C.redisHaxlNumThreads redisConfig

      mqttConfig   = C.mqttConfig conf


  pool <- C.genPSQLPool psqlConfig
  redis <- C.genRedisConnection redisConfig

  let state = stateSet (initRedisState redisThreads $ fromString prefix)
            $ stateSet (initDeviceState psqlThreads) stateEmpty

  let u = simpleEnv pool (fromString prefix) $ C.mkCache redis

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }

  _ <- runIO u state createTable

  mqtt <- startMQTT prefix mqttConfig $ \uuid bs ->
    runIO u state (updateDeviceMetaByUUID uuid bs)

  scottyOptsT opts (runIO u state) (application mqtt)
  where runIO :: HasPSQL u => u -> StateStore -> GenHaxl u w b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: (HasPSQL u, HasOtherEnv C.Cache u) => MqttEnv -> ScottyH u w ()
application mqtt = do
  middleware logStdout

  post "/api/devices/" createDeviceHandler
  post "/api/users/:username/devices/" createDeviceHandler

  post "/api/devices/:uuidOrToken/token/" $
    requireDevice updateDeviceTokenHandler
  post "/api/users/:username/devices/:uuidOrToken/token/" $
    requireDevice $ requireOwner updateDeviceTokenHandler

  post "/api/devices/:uuidOrToken/type/" $
    requireDevice updateDeviceTypeHandler
  post "/api/users/:username/devices/:uuidOrToken/type/" $
    requireDevice $ requireOwner updateDeviceTypeHandler

  post "/api/devices/:uuidOrToken/meta/" $
    requireDevice updateDeviceMetaHandler
  post "/api/users/:username/devices/:uuidOrToken/meta/" $
    requireDevice $ requireOwner updateDeviceMetaHandler

  post "/api/devices/:uuidOrToken/username/" $
    requireDevice updateDeviceUserNameHandler

  get "/api/devices/" getDeviceListHandler

  get "/api/users/:username/devices/" getDeviceListByNameHandler

  delete "/api/devices/:uuidOrToken/" $
    requireDevice removeDeviceHandler
  delete "/api/users/:username/devices/:uuidOrToken/" $
    requireDevice $ requireOwner removeDeviceHandler

  get "/api/devices/:uuidOrToken/" $
    requireDevice getDeviceHandler
  get "/api/users/:username/devices/:uuidOrToken/" $
    requireDevice $ requireOwner getDeviceHandler

  post "/api/devices/:uuidOrToken/rpc/" $
    requireDevice $ rpcHandler mqtt
  post "/api/users/:username/devices/:uuidOrToken/rpc/" $
    requireDevice $ requireOwner $ rpcHandler mqtt
