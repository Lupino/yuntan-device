{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Device.DataSource
  ( DeviceReq(..)
  , initDeviceState
  , setTablePrefix
  ) where


import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception          (SomeException, bracket_, try)
import           Data.Hashable              (Hashable (..))
import           Data.Int                   (Int64)
import           Data.IORef                 (IORef, newIORef, readIORef,
                                             writeIORef)
import           Data.Maybe                 (fromMaybe)
import           Data.Pool                  (withResource)
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           Database.PostgreSQL.Simple (Connection)
import           Database.PSQL.Types        (From, HasPSQL, OrderBy, PSQL,
                                             PSQLPool, Size, TablePrefix,
                                             psqlPool, runPSQL, tablePrefix)
import           Device.DataSource.Device
import           Device.DataSource.Table
import           Device.Types
import           Haxl.Core                  hiding (env, fetchReq)

-- Data source implementation.

data DeviceReq a where
  CreateTable :: DeviceReq Int64
  CreateDevice :: UserName -> Token -> DeviceReq DeviceID
  GetDevice :: DeviceID -> DeviceReq (Maybe Device)
  GetDevIdByToken :: Token -> DeviceReq (Maybe DeviceID)
  GetDevIdByUuid :: UUID -> DeviceReq (Maybe DeviceID)
  GetDevIdList :: From -> Size -> OrderBy -> DeviceReq [DeviceID]
  CountDevice :: DeviceReq Int64
  GetDevIdListByName :: UserName -> From -> Size -> OrderBy -> DeviceReq [DeviceID]
  CountDeviceByName :: UserName -> DeviceReq Int64
  UpdateDevice :: DeviceID -> String -> Text -> DeviceReq Int64
  RemoveDevice :: DeviceID -> DeviceReq Int64

  deriving (Typeable)

deriving instance Eq (DeviceReq a)
instance Hashable (DeviceReq a) where
  hashWithSalt s CreateTable                   = hashWithSalt s (1::Int)
  hashWithSalt s (CreateDevice u t)            = hashWithSalt s (2::Int, u, t)
  hashWithSalt s (GetDevice i)                 = hashWithSalt s (3::Int, i)
  hashWithSalt s (GetDevIdByToken t)           = hashWithSalt s (4::Int, t)
  hashWithSalt s (GetDevIdByUuid u)            = hashWithSalt s (5::Int, u)
  hashWithSalt s (GetDevIdList f si o)         = hashWithSalt s (6::Int, f, si, o)
  hashWithSalt s CountDevice                   = hashWithSalt s (7::Int)
  hashWithSalt s (GetDevIdListByName u f si o) = hashWithSalt s (8::Int, u, f, si, o)
  hashWithSalt s (CountDeviceByName u)         = hashWithSalt s (9::Int, u)
  hashWithSalt s (UpdateDevice i f t)          = hashWithSalt s (10::Int, i, f, t)
  hashWithSalt s (RemoveDevice i)              = hashWithSalt s (11::Int, i)

deriving instance Show (DeviceReq a)
instance ShowP DeviceReq where showp = show

instance StateKey DeviceReq where
  data State DeviceReq = DeviceState { numThreads :: Int, statePrefix :: IORef (Maybe TablePrefix) }

instance DataSourceName DeviceReq where
  dataSourceName _ = "DeviceDataSource"

instance HasPSQL u => DataSource u DeviceReq where
  fetch = doFetch

doFetch
  :: HasPSQL u
  => State DeviceReq
  -> Flags
  -> u
  -> PerformFetch DeviceReq

doFetch _state _flags _user = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  prefix <- fromMaybe (tablePrefix _user) <$> readIORef (statePrefix _state)
  asyncs <- mapM (fetchAsync sem (psqlPool _user) prefix) reqs
  inner
  mapM_ wait asyncs

fetchAsync :: QSem -> PSQLPool -> TablePrefix -> BlockedFetch DeviceReq -> IO (Async ())
fetchAsync sem pool prefix req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

fetchSync :: BlockedFetch DeviceReq -> TablePrefix -> Connection -> IO ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ runPSQL prefix conn (fetchReq req)
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: DeviceReq a -> PSQL a
fetchReq CreateTable                   = createTable
fetchReq (CreateDevice u t)            = createDevice u t
fetchReq (GetDevice i)                 = getDevice i
fetchReq (GetDevIdByToken t)           = getDevIdByToken t
fetchReq (GetDevIdByUuid u)            = getDevIdByUuid u
fetchReq (GetDevIdList f si o)         = getDevIdList f si o
fetchReq CountDevice                   = countDevice
fetchReq (GetDevIdListByName u f si o) = getDevIdListByName u f si o
fetchReq (CountDeviceByName u)         = countDeviceByName u
fetchReq (UpdateDevice i f t)          = updateDevice i f t
fetchReq (RemoveDevice i)              = removeDevice i

initDeviceState :: Int -> IO (State DeviceReq)
initDeviceState threads = do
  ref <- newIORef Nothing
  return $ DeviceState threads ref


setTablePrefix :: State DeviceReq -> TablePrefix -> IO ()
setTablePrefix ds prefix = writeIORef (statePrefix ds) (Just prefix)
