{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Concurrent.QSem
import qualified Control.Exception                    as CE (bracket_)
import           Control.Monad                        (void, when)
import           Data.ByteString                      (ByteString)
import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.Exit                          (exitSuccess)
import           Web.Scotty.Trans                     (delete, get, middleware,
                                                       post, scottyOptsT,
                                                       settings)

import           Data.String                          (fromString)
import           Database.PSQL                        (HasOtherEnv, HasPSQL,
                                                       SimpleEnv, simpleEnv)
import           Haxl.RedisCache                      (initRedisState)
import           Web.Scotty.Haxl                      (ScottyH)

import           Device
import qualified Device.Auth                          as Auth
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

  let psqlConfig   = C.psqlConfig conf
      psqlMaxPool  = C.psqlPoolMaxResources psqlConfig
      redisConfig  = C.redisConfig conf
      redisMaxConn = C.redisMaxConnections redisConfig

      mqttConfig   = C.mqttConfig conf
      emqxAuth     = C.emqxAuth conf
      allowKeys    = C.allowKeys conf
      authEnable   = C.authEnable conf
      authKey      = C.authKey conf
      qps          = C.maxQPS conf

  sem <- newQSem qps

  pool <- C.genPSQLPool psqlConfig
  redis <- C.genRedisConnection redisConfig

  let u = simpleEnv pool (fromString prefix) $ C.mkCache redis
      s = stateSet (initRedisState redisMaxConn $ fromString prefix)
        $ stateSet (initDeviceState psqlMaxPool $ fromString prefix)
        stateEmpty
      runIO0 = CE.bracket_ (waitQSem sem) (signalQSem sem) . runIO u s

      opts = def
        { settings = setPort port
                   $ setHost (Host host) (settings def)
        }


  runIO0 $ void createTable

  when dryRun exitSuccess

  mqtt <- startMQTT allowKeys mqttConfig $ \tp uuid bs ->
    runIO0 $ updateDeviceMetaByUUID tp uuid bs

  scottyOptsT opts runIO0 (application mqtt emqxAuth authEnable authKey)
  where runIO :: SimpleEnv C.Cache -> StateStore -> GenHaxl (SimpleEnv C.Cache) () b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application
  :: (HasPSQL u, HasOtherEnv C.Cache u, Monoid w)
  => MqttEnv -> Maybe C.EmqxAuthConfig -> Bool -> ByteString -> ScottyH u w ()
application mqtt mEmqxAuth authEnable authKey = do
  middleware logStdout

  post "/api/devices/"                             $ requireAdmin $ createDeviceHandler allowKeys
  post "/api/devices/:ident/token/"                $ rad $ updateDeviceHandler "token"
  post "/api/devices/:ident/uuid/"                 $ rad $ updateDeviceHandler "uuid"
  post "/api/devices/:ident/addr/"                 $ rad $ updateDeviceHandler "addr"
  post "/api/devices/:ident/gw_id/"                $ rad $ updateDeviceHandler "gw_id"
  post "/api/devices/:ident/created_at/"           $ rad $ updateDeviceHandler "created_at"
  post "/api/devices/:ident/ping_at/"              $ rmd updateDevicePingAtHandler
  post "/api/devices/:ident/meta/"                 $ rmd updateDeviceMetaHandler

  get "/api/devices/"                              $ requireIndexName $ getDeviceListHandler allowKeys
  get "/api/devices/:ident/"                       $ rdp getDeviceHandler
  post "/api/devices/:ident/rpc/"                  $ rdp $ rpcHandler mqtt

  post "/api/devices/:ident/metric/"               $ rmd saveMetricHandler
  post "/api/devices/:ident/cards/"                $ rmd saveCardHandler

  get "/api/devices/:ident/metric/:param/"         $ rdp getMetricListHandler

  delete "/api/devices/:ident/"                    $ rmd (removeDeviceHandler mqtt)
  delete "/api/devices/:ident/metric/:param/"      $ rmd dropMetricHandler
  delete "/api/devices/:ident/cards/:param/"       $ rmd removeCardHandler
  delete "/api/devices/:ident/metric/:param/:mid/" $ rmd removeMetricHandler

  post "/api/devices/:ident/index/"                $ rmd saveIndexHandler
  post "/api/devices/:ident/index/delete/"         $ rmd removeIndexHandler
  post "/api/devices/:ident/index/drop/"           $ rmd dropDeviceIndexHandler
  post "/api/index/drop/"                          $ requireAdmin dropIndexHandler
  post "/api/gen_token/"                           $ requireAdmin $ Auth.genTokenHandler authKey

  case mEmqxAuth of
    Nothing -> pure ()
    Just emqxAuth -> do
      post "/mqtt/acl" emqxAclReqHandler
      post "/mqtt/superuser" emqxSuperReqHandler
      post "/mqtt/auth" $ emqxAuthReqHandler emqxAuth
      post "/emqx5/auth" $ emqx5AuthReqHandler emqxAuth

  where allowKeys = mAllowKeys mqtt
        requireAdmin = Auth.requireAdmin authEnable authKey
        requirePerm = Auth.requirePerm authEnable authKey
        requireManager = Auth.requireManager authEnable authKey
        requireIndexName = Auth.requireIndexName authEnable authKey
        rdp = requireDevice . requirePerm
        rad = requireAdmin . requireDevice
        rmd =  requireDevice . requireManager
