{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad                        (forM_, void, when)
import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Data.Text                            (pack, unpack)
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
import           Haxl.Core                            (GenHaxl, initEnv,
                                                       runHaxl, stateEmpty,
                                                       stateSet)

import qualified Data.Yaml                            as Y
import qualified Device.Config                        as C
import           Device.MQTT                          (MqttEnv, startMQTT)

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
program Options { getConfigFile  = confFile
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
      runIO0 = runIO u psqlThreads redisThreads

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }


  runIO0 $ forM_ (pack prefix:allowKeys) $ \tp -> do
    setTablePrefix $ unpack tp
    void createTable

  when dryRun exitSuccess

  mqtt <- startMQTT (pack prefix:allowKeys) mqttConfig $ \key uuid bs force ->
    runIO0 $ do
      setTablePrefix $ unpack key
      updateDeviceMetaByUUID uuid bs force

  scottyOptsT opts runIO0 (application mqtt)
  where runIO :: SimpleEnv C.Cache -> Int -> Int -> GenHaxl (SimpleEnv C.Cache) () b -> IO b
        runIO env psqlThreads redisThreads m = do
          redisState <- initRedisState redisThreads $ fromString prefix
          deviceState <- initDeviceState psqlThreads
          let s = stateSet redisState $ stateSet deviceState stateEmpty

          env0 <- initEnv s env
          runHaxl env0 m

application :: (HasPSQL u, HasOtherEnv C.Cache u, Monoid w) => MqttEnv -> ScottyH u w ()
application mqtt = do
  middleware logStdout

  post "/api/devices/" $ addKey createDeviceHandler
  post "/api/users/:username/devices/" $ addKey createDeviceHandler

  post "/api/devices/:uuidOrToken/token/" $ addKey $
    requireDevice updateDeviceTokenHandler
  post "/api/users/:username/devices/:uuidOrToken/token/" $ addKey $
    requireDevice $ requireOwner updateDeviceTokenHandler

  post "/api/devices/:uuidOrToken/meta/" $ addKey $
    requireDevice updateDeviceMetaHandler
  post "/api/users/:username/devices/:uuidOrToken/meta/" $ addKey $
    requireDevice $ requireOwner updateDeviceMetaHandler

  post "/api/devices/:uuidOrToken/username/" $ addKey $
    requireDevice updateDeviceUserNameHandler

  get "/api/devices/" $ addKey getDeviceListHandler

  get "/api/users/:username/devices/" $ addKey getDeviceListByNameHandler

  delete "/api/devices/:uuidOrToken/" $ addKey $
    requireDevice (removeDeviceHandler mqtt)
  delete "/api/users/:username/devices/:uuidOrToken/" $ addKey $
    requireDevice $ requireOwner (removeDeviceHandler mqtt)

  get "/api/devices/:uuidOrToken/" $ addKey $
    requireDevice getDeviceHandler
  get "/api/users/:username/devices/:uuidOrToken/" $ addKey $
    requireDevice $ requireOwner getDeviceHandler

  post "/api/devices/:uuidOrToken/rpc/" $ addKey $
    requireDevice $ rpcHandler mqtt
  post "/api/users/:username/devices/:uuidOrToken/rpc/" $ addKey $
    requireDevice $ requireOwner $ rpcHandler mqtt
