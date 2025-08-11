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


import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception        as CE (SomeException, bracket_, try)
import           Data.Hashable            (Hashable (..))
import           Data.Int                 (Int64)
import           Data.List                (groupBy)
import           Data.Text                (Text)
import           Data.Typeable            (Typeable)
import           Database.PSQL.Types      (From, HasPSQL, OrderBy, PSQL,
                                           PSQLPool, Size, TablePrefix,
                                           psqlPool, runPSQLPool)
import           Device.DataSource.Device
import           Device.DataSource.Metric
import           Device.DataSource.Table
import           Device.Types
import           Haxl.Core                hiding (env, fetchReq)

-- Data source implementation.

data DeviceReq a where
  CreateTable :: DeviceReq Int64
  CreateDevice :: KeyID -> Token -> Addr -> DeviceReq DeviceID
  GetDevice :: DeviceID -> DeviceReq (Maybe Device)
  GetDevIdByCol :: String -> Text -> DeviceReq (Maybe DeviceID)
  GetDevIdList :: From -> Size -> OrderBy -> DeviceReq [DeviceID]
  CountDevice :: DeviceReq Int64
  GetDevIdListByKey :: KeyID -> From -> Size -> OrderBy -> DeviceReq [DeviceID]
  CountDeviceByKey :: KeyID -> DeviceReq Int64
  UpdateDevice :: DeviceID -> String -> Text -> DeviceReq Int64
  RemoveDevice :: DeviceID -> DeviceReq Int64
  GetDevKeyID :: Key -> DeviceReq KeyID
  GetDevKeyByID :: KeyID -> DeviceReq Key
  GetDevIdListByGw :: DeviceID -> From -> Size -> OrderBy -> DeviceReq [DeviceID]
  CountDevAddrByGw :: DeviceID -> DeviceReq Int64

  SaveMetric :: DeviceID -> String -> String -> Float -> CreatedAt -> DeviceReq Int64
  GetMetric :: MetricID -> DeviceReq (Maybe Metric)
  GetMetricIdList :: DeviceID -> String -> Int64 -> Int64 -> From -> Size -> OrderBy -> DeviceReq [MetricID]
  CountMetric :: DeviceID -> String -> Int64 -> Int64 -> DeviceReq Int64
  RemoveMetric :: MetricID -> DeviceReq Int64
  GetLastMetricIdList :: DeviceID -> DeviceReq [(String, MetricID)]

  deriving (Typeable)

deriving instance Eq (DeviceReq a)
instance Hashable (DeviceReq a) where
  hashWithSalt s CreateTable                     = hashWithSalt s (1::Int)
  hashWithSalt s (CreateDevice k t a)            = hashWithSalt s (2::Int, k, t, a)
  hashWithSalt s (GetDevice i)                   = hashWithSalt s (3::Int, i)
  hashWithSalt s (GetDevIdByCol c v)             = hashWithSalt s (4::Int, c, v)
  hashWithSalt s (GetDevIdList f si o)           = hashWithSalt s (6::Int, f, si, o)
  hashWithSalt s CountDevice                     = hashWithSalt s (7::Int)
  hashWithSalt s (GetDevIdListByKey k f si o)    = hashWithSalt s (8::Int, k, f, si, o)
  hashWithSalt s (CountDeviceByKey k)            = hashWithSalt s (9::Int, k)
  hashWithSalt s (UpdateDevice i f t)            = hashWithSalt s (10::Int, i, f, t)
  hashWithSalt s (RemoveDevice i)                = hashWithSalt s (11::Int, i)
  hashWithSalt s (GetDevKeyID k)                 = hashWithSalt s (12::Int, k)
  hashWithSalt s (GetDevKeyByID k)               = hashWithSalt s (13::Int, k)
  hashWithSalt s (GetDevIdListByGw g f si o)     = hashWithSalt s (17::Int, g, f, si, o)
  hashWithSalt s (CountDevAddrByGw g)            = hashWithSalt s (18::Int, g)

  hashWithSalt s (SaveMetric a b c d e)          = hashWithSalt s (19::Int, a, b, c, d, e)
  hashWithSalt s (GetMetric a)                   = hashWithSalt s (21::Int, a)
  hashWithSalt s (GetMetricIdList a b c d e f g) = hashWithSalt s (22::Int, a, b, c, d, e, (f, g))
  hashWithSalt s (CountMetric a b c d)           = hashWithSalt s (23::Int, a, b, c, d)
  hashWithSalt s (RemoveMetric a)                = hashWithSalt s (24::Int, a)
  hashWithSalt s (GetLastMetricIdList a)         = hashWithSalt s (25::Int, a)

deriving instance Show (DeviceReq a)
instance ShowP DeviceReq where showp = show

instance StateKey DeviceReq where
  data State DeviceReq = DeviceState { numThreads :: Int, statePrefix :: TablePrefix }

instance DataSourceName DeviceReq where
  dataSourceName _ = "DeviceDataSource"

instance HasPSQL u => DataSource u DeviceReq where
  fetch = doFetch

isSameType :: BlockedFetch DeviceReq -> BlockedFetch DeviceReq -> Bool
isSameType (BlockedFetch (GetDevice _) _) (BlockedFetch (GetDevice _) _) = True
isSameType (BlockedFetch (GetDevKeyByID _) _) (BlockedFetch (GetDevKeyByID _) _) = True
isSameType (BlockedFetch (GetDevIdByCol c0 _) _) (BlockedFetch (GetDevIdByCol c1 _) _) = c0 == c1
isSameType (BlockedFetch (GetMetric _) _) (BlockedFetch (GetMetric _) _) = True
isSameType (BlockedFetch (GetLastMetricIdList _) _) (BlockedFetch (GetLastMetricIdList _) _) = True
isSameType _ _ = False

doFetch
  :: HasPSQL u
  => State DeviceReq
  -> Flags
  -> u
  -> PerformFetch DeviceReq

doFetch _state _flags _user = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem (psqlPool _user) (statePrefix _state)) (groupBy isSameType reqs)
  inner
  mapM_ wait asyncs

fetchAsync :: QSem -> PSQLPool -> TablePrefix -> [BlockedFetch DeviceReq] -> IO (Async ())
fetchAsync sem pool prefix req = async $
  CE.bracket_ (waitQSem sem) (signalQSem sem) $ fetchSync req prefix pool


putFail :: CE.SomeException -> BlockedFetch DeviceReq -> IO ()
putFail ex (BlockedFetch _ rvar) = putFailure rvar ex


fetchSync :: [BlockedFetch DeviceReq] -> TablePrefix -> PSQLPool -> IO ()
fetchSync [] _ _ = return ()
fetchSync [BlockedFetch req rvar] prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (fetchReq req)
  case e of
    Left ex -> putFailure rvar (ex :: CE.SomeException)
    Right a -> putSuccess rvar a

fetchSync reqs@((BlockedFetch (GetDevice _) _):_) prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (getDeviceList ids)
  case e of
    Left ex -> mapM_ (putFail ex) reqs
    Right a ->  mapM_ (putReq a) reqs

  where ids = [i | BlockedFetch (GetDevice i) _ <- reqs]
        putReq :: [Device] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetDevice _) rvar) = putSuccess rvar Nothing
        putReq (x:xs) req@(BlockedFetch (GetDevice i) rvar)
          | i == devID x = putSuccess rvar (Just x)
          | otherwise = putReq xs req

        putReq _ _ = return ()

fetchSync reqs@((BlockedFetch (GetDevKeyByID _) _):_) prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (getDevKeyList ids)
  case e of
    Left ex -> mapM_ (putFail ex) reqs
    Right a ->  mapM_ (putReq a) reqs

  where ids = [i | BlockedFetch (GetDevKeyByID i) _ <- reqs]
        putReq :: [(KeyID, Key)] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetDevKeyByID _) rvar) = putSuccess rvar ""
        putReq (x:xs) req@(BlockedFetch (GetDevKeyByID i) rvar)
          | i == fst x = putSuccess rvar (snd x)
          | otherwise = putReq xs req

        putReq _ _ = return ()

fetchSync reqs@((BlockedFetch (GetDevIdByCol col _) _):_) prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (getDevIdListByCol col vals)
  case e of
    Left ex -> mapM_ (putFail ex) reqs
    Right a ->  mapM_ (putReq a) reqs

  where vals = [v | BlockedFetch (GetDevIdByCol _ v) _ <- reqs]
        putReq :: [(Text, DeviceID)] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetDevIdByCol _ _) rvar) = putSuccess rvar Nothing
        putReq (x:xs) req@(BlockedFetch (GetDevIdByCol _ c) rvar)
          | c == fst x = putSuccess rvar (Just (snd x))
          | otherwise = putReq xs req

        putReq _ _ = return ()

fetchSync reqs@((BlockedFetch (GetMetric _) _):_) prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (getMetricList ids)
  case e of
    Left ex -> mapM_ (putFail ex) reqs
    Right a ->  mapM_ (putReq a) reqs

  where ids = [i | BlockedFetch (GetMetric i) _ <- reqs]
        putReq :: [Metric] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetMetric _) rvar) = putSuccess rvar Nothing
        putReq (x:xs) req@(BlockedFetch (GetMetric i) rvar)
          | i == metricId x = putSuccess rvar (Just x)
          | otherwise = putReq xs req

        putReq _ _ = return ()

fetchSync reqs@((BlockedFetch (GetLastMetricIdList _) _):_) prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (getLastMetricIdList' ids)
  case e of
    Left ex -> mapM_ (putFail ex) reqs
    Right a ->  mapM_ (putReq (groupBy isSameDev a)) reqs

  where ids = [i | BlockedFetch (GetLastMetricIdList i) _ <- reqs]

        isSameDev :: (DeviceID, String, MetricID) -> (DeviceID, String, MetricID) -> Bool
        isSameDev (x, _, _) (y, _, _) = x == y

        rmDid :: (DeviceID, String, MetricID) -> (String, MetricID)
        rmDid (_, x, y) = (x, y)

        putReq :: [[(DeviceID, String, MetricID)]] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetLastMetricIdList _) rvar) = putSuccess rvar []
        putReq ([]:xs) req = putReq xs req
        putReq (x@((did, _, _):_):xs) req@(BlockedFetch (GetLastMetricIdList i) rvar)
          | i == did = putSuccess rvar (map rmDid x)
          | otherwise = putReq xs req

        putReq _ _ = return ()

fetchSync reqs prefix pool = mapM_ (\x -> fetchSync [x] prefix pool) reqs

fetchReq :: DeviceReq a -> PSQL a
fetchReq CreateTable                     = createTable
fetchReq (CreateDevice k t a)            = createDevice k t a
fetchReq (GetDevice i)                   = getDevice i
fetchReq (GetDevIdByCol c v)             = getDevIdByCol c v
fetchReq (GetDevIdList f si o)           = getDevIdList f si o
fetchReq CountDevice                     = countDevice
fetchReq (GetDevIdListByKey k f si o)    = getDevIdListByKey k f si o
fetchReq (CountDeviceByKey k)            = countDeviceByKey k
fetchReq (UpdateDevice i f t)            = updateDevice i f t
fetchReq (RemoveDevice i)                = removeDevice i
fetchReq (GetDevKeyID k)                 = getDevKeyId k
fetchReq (GetDevKeyByID k)               = getDevKeyById k
fetchReq (GetDevIdListByGw g f si o)     = getDevIdListByGw g f si o
fetchReq (CountDevAddrByGw g)            = countDevAddrByGw g

fetchReq (SaveMetric a b c d e)          = saveMetric a b c d e
fetchReq (GetMetric a)                   = getMetric a
fetchReq (GetMetricIdList a b c d e f g) = getMetricIdList a b c d e f g
fetchReq (CountMetric a b c d)           = countMetric a b c d
fetchReq (RemoveMetric a)                = removeMetric a
fetchReq (GetLastMetricIdList a)         = getLastMetricIdList a

initDeviceState :: Int -> TablePrefix -> State DeviceReq
initDeviceState = DeviceState
