{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad                        (void, when)
import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.Exit                          (exitSuccess)
import           Web.Scotty.Trans                     (delete, get, middleware,
                                                       post, scottyOptsT,
                                                       settings)

import           Data.String                          (fromString)
import           Database.PSQL.Types                  (HasOtherEnv, HasPSQL,
                                                       SimpleEnv, simpleEnv)
import           Haxl.RedisCache                      (initRedisState)
import           Web.Scotty.Haxl                      (ScottyH)

import           Device
import           Device.Handler
import           Haxl.Core                            (GenHaxl, StateStore,
                                                       initEnv, runHaxl,
                                                       stateEmpty, stateSet)

import qualified Data.Yaml                            as Y
import qualified Device.Config                        as C
import           Device.MQTT                          (MqttEnv (mAllowKeys),
                                                       startMQTT)

import           Options.Applicative

data Options = Options
    { getConfigFile  :: String
    , getHost        :: String
    , getPort        :: Int
    , getTablePrefix :: String
    , getDryRun      :: Bool
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
  <*> switch    (long "dry-run"
                 <> help "only create tables.")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Device micro server"
     <> header "yuntan-device - Device micro server" )

program :: Options -> IO ()
program Options
  { getConfigFile  = confFile
  , getTablePrefix = prefix
  , getHost        = host
  , getPort        = port
  , getDryRun      = dryRun
  } = do

  (Right conf) <- Y.decodeFileEither confFile

  let psqlConfig  = C.psqlConfig conf
      psqlThreads = C.psqlHaxlNumThreads psqlConfig
      redisConfig  = C.redisConfig conf
      redisThreads = C.redisHaxlNumThreads redisConfig

      mqttConfig   = C.mqttConfig conf
      allowKeys    = C.allowKeys conf


  pool <- C.genPSQLPool psqlConfig
  redis <- C.genRedisConnection redisConfig

  let u = simpleEnv pool (fromString prefix) $ C.mkCache redis
      s = stateSet (initRedisState redisThreads $ fromString prefix)
        $ stateSet (initDeviceState psqlThreads $ fromString prefix)
        stateEmpty
      runIO0 = runIO u s

      opts = def
        { settings = setPort port
                   $ setHost (Host host) (settings def)
        }


  runIO0 $ void createTable

  when dryRun exitSuccess

  mqtt <- startMQTT (fromString prefix:allowKeys) mqttConfig $ \_ uuid bs force ->
    runIO0 $ updateDeviceMetaByUUID uuid bs force

  scottyOptsT opts runIO0 (application mqtt)
  where runIO :: SimpleEnv C.Cache -> StateStore -> GenHaxl (SimpleEnv C.Cache) () b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: (HasPSQL u, HasOtherEnv C.Cache u, Monoid w) => MqttEnv -> ScottyH u w ()
application mqtt = do
  middleware logStdout

  post "/api/devices/" $ createDeviceHandler allowKeys
  post "/api/devices/:ident/token/" $ requireDevice $ updateDeviceHandler "token"
  post "/api/devices/:ident/uuid/" $ requireDevice $ updateDeviceHandler "uuid"
  post "/api/devices/:ident/addr/" $ requireDevice $ updateDeviceHandler "addr"
  post "/api/devices/:ident/gw_id/" $ requireDevice $ updateDeviceHandler "gw_id"
  post "/api/devices/:ident/created_at/" $ requireDevice $ updateDeviceHandler "created_at"
  post "/api/devices/:ident/ping_at/" $ requireDevice $ updateDevicePingAtHandler
  post "/api/devices/:ident/meta/" $ requireDevice updateDeviceMetaHandler
  get "/api/devices/" $ getDeviceListHandler allowKeys
  delete "/api/devices/:ident/" $ requireDevice (removeDeviceHandler mqtt)
  get "/api/devices/:ident/" $ requireDevice getDeviceHandler
  post "/api/devices/:ident/rpc/" $ requireDevice $ rpcHandler mqtt

  where allowKeys = mAllowKeys mqtt
