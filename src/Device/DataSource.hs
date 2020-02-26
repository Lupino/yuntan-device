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
  ) where

import           Data.Hashable              (Hashable (..))
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           Haxl.Core                  hiding (env, fetchReq)

import           Device.DataSource.Device
import           Device.DataSource.Table
import           Device.Types
import           Yuntan.Types.HasPSQL       (HasPSQL, TablePrefix, psqlPool,
                                             tablePrefix)
import           Yuntan.Types.ListResult    (From, Size)
import           Yuntan.Types.OrderBy       (OrderBy)

import qualified Control.Exception          (SomeException, bracket_, try)
import           Data.Int                   (Int64)
import           Data.Pool                  (withResource)
import           Database.PostgreSQL.Simple (Connection)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data DeviceReq a where
  CreateTable :: DeviceReq Int64
  CreateDevice :: UserName -> Token -> Type -> DeviceReq DeviceID
  GetDevice :: DeviceID -> DeviceReq (Maybe Device)
  GetDevIdByToken :: Token -> DeviceReq (Maybe DeviceID)
  GetDevIdByUuid :: UUID -> DeviceReq (Maybe DeviceID)
  GetDevIdList :: From -> Size -> OrderBy -> DeviceReq [DeviceID]
  CountDevice :: DeviceReq Int64
  GetDevIdListByName :: UserName -> From -> Size -> OrderBy -> DeviceReq [DeviceID]
  CountDeviceByName :: UserName -> DeviceReq Int64
  GetDevIdListByType :: Type -> From -> Size -> OrderBy -> DeviceReq [DeviceID]
  CountDeviceByType :: Type -> DeviceReq Int64
  GetDevIdListByNameAndType :: UserName -> Type -> From -> Size -> OrderBy -> DeviceReq [DeviceID]
  CountDeviceByNameAndType :: UserName -> Type -> DeviceReq Int64
  UpdateDevice :: DeviceID -> String -> Text -> DeviceReq Int64
  RemoveDevice :: DeviceID -> DeviceReq Int64

  deriving (Typeable)

deriving instance Eq (DeviceReq a)
instance Hashable (DeviceReq a) where
  hashWithSalt s CreateTable                            = hashWithSalt s (1::Int)
  hashWithSalt s (CreateDevice u t tp)                  = hashWithSalt s (2::Int, u, t, tp)
  hashWithSalt s (GetDevice i)                          = hashWithSalt s (3::Int, i)
  hashWithSalt s (GetDevIdByToken t)                    = hashWithSalt s (4::Int, t)
  hashWithSalt s (GetDevIdByUuid u)                     = hashWithSalt s (5::Int, u)
  hashWithSalt s (GetDevIdList f si o)                  = hashWithSalt s (6::Int, f, si, o)
  hashWithSalt s CountDevice                            = hashWithSalt s (7::Int)
  hashWithSalt s (GetDevIdListByName u f si o)          = hashWithSalt s (8::Int, u, f, si, o)
  hashWithSalt s (CountDeviceByName u)                  = hashWithSalt s (9::Int, u)
  hashWithSalt s (GetDevIdListByType t f si o)          = hashWithSalt s (10::Int, t, f, si, o)
  hashWithSalt s (CountDeviceByType t)                  = hashWithSalt s (11::Int, t)
  hashWithSalt s (GetDevIdListByNameAndType u t f si o) = hashWithSalt s (12::Int, u, t, f, si, o)
  hashWithSalt s (CountDeviceByNameAndType u t)         = hashWithSalt s (13::Int, u, t)
  hashWithSalt s (UpdateDevice i f t)                   = hashWithSalt s (16::Int, i, f, t)
  hashWithSalt s (RemoveDevice i)                       = hashWithSalt s (17::Int, i)

deriving instance Show (DeviceReq a)
instance ShowP DeviceReq where showp = show

instance StateKey DeviceReq where
  data State DeviceReq = DeviceState { numThreads :: Int }

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
  asyncs <- mapM (fetchAsync sem _user) reqs
  inner
  mapM_ wait asyncs

fetchAsync :: HasPSQL u => QSem -> u -> BlockedFetch DeviceReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem)
  $ withResource pool
  $ fetchSync req prefix

  where pool   = psqlPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch DeviceReq -> TablePrefix -> Connection -> IO ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: DeviceReq a -> TablePrefix -> Connection -> IO a
fetchReq CreateTable = createTable
fetchReq (CreateDevice u t tp)                  = createDevice u t tp
fetchReq (GetDevice i)                          = getDevice i
fetchReq (GetDevIdByToken t)                    = getDevIdByToken t
fetchReq (GetDevIdByUuid u)                     = getDevIdByUuid u
fetchReq (GetDevIdList f si o)                  = getDevIdList f si o
fetchReq CountDevice                            = countDevice
fetchReq (GetDevIdListByName u f si o)          = getDevIdListByName u f si o
fetchReq (CountDeviceByName u)                  = countDeviceByName u
fetchReq (GetDevIdListByType t f si o)          = getDevIdListByType t f si o
fetchReq (CountDeviceByType t)                  = countDeviceByType t
fetchReq (GetDevIdListByNameAndType u t f si o) = getDevIdListByNameAndType u t f si o
fetchReq (CountDeviceByNameAndType u t)         = countDeviceByNameAndType u t
fetchReq (UpdateDevice i f t)                   = updateDevice i f t
fetchReq (RemoveDevice i)                       = removeDevice i

initDeviceState :: Int -> State DeviceReq
initDeviceState = DeviceState
