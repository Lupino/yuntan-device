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

  , devices
  , deviceKeys
  , cards

  , metrics

  , indexs
  , indexNames
  ) where


import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception        as CE (SomeException, bracket_, try)
import           Data.Hashable            (Hashable (..))
import           Data.Int                 (Int64)
import           Data.List                (groupBy)
import           Data.Text                (Text)
import           Data.Typeable            (Typeable)
import           Database.PSQL            (Action, Columns, HasPSQL, PSQL,
                                           PSQLPool, Page, TableName,
                                           TablePrefix, psqlPool, runPSQLPool)
import           Device.DataSource.Card
import           Device.DataSource.Device
import           Device.DataSource.Index
import           Device.DataSource.Metric
import           Device.DataSource.Table
import           Device.DataSource.Util
import           Device.Types
import           Haxl.Core                hiding (env, fetchReq)

-- Data source implementation.

data DeviceReq a where
  CreateTable :: DeviceReq Int64
  GetDevice :: DeviceID -> DeviceReq (Maybe Device)
  GetDevKeyByID :: KeyID -> DeviceReq (Maybe Key)

  SaveMetric :: DeviceID -> Param -> String -> Float -> CreatedAt -> DeviceReq Int64
  GetMetric :: MetricID -> DeviceReq (Maybe Metric)
  GetLastMetricIdList :: DeviceID -> DeviceReq [(Param, MetricID)]

  SaveIndex :: IndexNameId -> DeviceID -> DeviceReq Int64
  GetIndexDevIdList :: [IndexNameId] -> Page -> DeviceReq [DeviceID]
  GetIndexList :: DeviceID -> DeviceReq [Index]

  AddOne :: TableName -> Columns -> [Action] -> DeviceReq Int64
  AddOne_ :: TableName -> Columns -> [Action] -> DeviceReq Int64
  UpdateById :: TableName -> Int64 -> Columns -> [Action] -> DeviceReq Int64
  RemoveBy :: TableName -> String -> [Action] -> DeviceReq Int64
  GetIdByCol :: TableName -> String -> Text -> DeviceReq (Maybe Int64)
  GetIdListBy :: TableName -> String -> [Action] -> Page -> DeviceReq [Int64]
  CountBy :: TableName -> String -> [Action] -> DeviceReq Int64
  GetIdListAll :: TableName -> Page -> DeviceReq [Int64]
  CountAll :: TableName -> DeviceReq Int64

  GetCard :: CardID -> DeviceReq (Maybe Card)

  deriving (Typeable)

deriving instance Eq (DeviceReq a)
instance Hashable (DeviceReq a) where
  hashWithSalt s CreateTable             = hashWithSalt s (1::Int)
  hashWithSalt s (GetDevice i)           = hashWithSalt s (2::Int, i)
  hashWithSalt s (GetDevKeyByID k)       = hashWithSalt s (3::Int, k)

  hashWithSalt s (SaveMetric a b c d e)  = hashWithSalt s (4::Int, a, b, c, d, e)
  hashWithSalt s (GetMetric a)           = hashWithSalt s (5::Int, a)
  hashWithSalt s (GetLastMetricIdList a) = hashWithSalt s (6::Int, a)

  hashWithSalt s (SaveIndex a b)         = hashWithSalt s (7::Int, a, b)
  hashWithSalt s (GetIndexDevIdList a b) = hashWithSalt s (8::Int, a, b)
  hashWithSalt s (GetIndexList a)        = hashWithSalt s (9::Int, a)

  hashWithSalt s (AddOne a b c)          = hashWithSalt s (10::Int, a, b, c)
  hashWithSalt s (AddOne_ a b c)         = hashWithSalt s (11::Int, a, b, c)
  hashWithSalt s (UpdateById a b c d)    = hashWithSalt s (12::Int, a, b, c, d)
  hashWithSalt s (RemoveBy a b c)        = hashWithSalt s (13::Int, a, b, c)
  hashWithSalt s (GetIdByCol a b c)      = hashWithSalt s (14::Int, a, b, c)
  hashWithSalt s (GetIdListBy a b c d)   = hashWithSalt s (15::Int, a, b, c, d)
  hashWithSalt s (CountBy a b c)         = hashWithSalt s (16::Int, a, b, c)
  hashWithSalt s (GetIdListAll a b)      = hashWithSalt s (17::Int, a, b)
  hashWithSalt s (CountAll a)            = hashWithSalt s (18::Int64, a)

  hashWithSalt s (GetCard a)             = hashWithSalt s (19::Int64, a)

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
isSameType (BlockedFetch (GetCard _) _) (BlockedFetch (GetCard _) _) = True
isSameType (BlockedFetch (GetDevKeyByID _) _) (BlockedFetch (GetDevKeyByID _) _) = True
isSameType (BlockedFetch (GetMetric _) _) (BlockedFetch (GetMetric _) _) = True
isSameType (BlockedFetch (GetLastMetricIdList _) _) (BlockedFetch (GetLastMetricIdList _) _) = True
isSameType (BlockedFetch (GetIndexList _) _) (BlockedFetch (GetIndexList _) _) = True
isSameType (BlockedFetch (GetIdByCol t0 c0 _) _) (BlockedFetch (GetIdByCol t1 c1 _) _) = c0 == c1 && t0 == t1
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
        putReq [] (BlockedFetch (GetDevKeyByID _) rvar) = putSuccess rvar Nothing
        putReq (x:xs) req@(BlockedFetch (GetDevKeyByID i) rvar)
          | i == fst x = putSuccess rvar (Just $ snd x)
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

        isSameDev :: (DeviceID, Param, MetricID) -> (DeviceID, Param, MetricID) -> Bool
        isSameDev (x, _, _) (y, _, _) = x == y

        rmDid :: (DeviceID, Param, MetricID) -> (Param, MetricID)
        rmDid (_, x, y) = (x, y)

        putReq :: [[(DeviceID, Param, MetricID)]] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetLastMetricIdList _) rvar) = putSuccess rvar []
        putReq ([]:xs) req = putReq xs req
        putReq (x@((did, _, _):_):xs) req@(BlockedFetch (GetLastMetricIdList i) rvar)
          | i == did = putSuccess rvar (map rmDid x)
          | otherwise = putReq xs req

        putReq _ _ = return ()

fetchSync reqs@((BlockedFetch (GetIndexList _) _):_) prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (getIndexList ids)
  case e of
    Left ex -> mapM_ (putFail ex) reqs
    Right a ->  mapM_ (putReq (groupBy isSameDev a)) reqs

  where ids = [i | BlockedFetch (GetIndexList i) _ <- reqs]

        isSameDev :: Index -> Index -> Bool
        isSameDev x y = indexDevId x == indexDevId y

        putReq :: [[Index]] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetIndexList _) rvar) = putSuccess rvar []
        putReq ([]:xs) req = putReq xs req
        putReq (x@(index:_):xs) req@(BlockedFetch (GetIndexList i) rvar)
          | i == indexDevId index = putSuccess rvar x
          | otherwise = putReq xs req

        putReq _ _ = return ()

fetchSync reqs@((BlockedFetch (GetIdByCol tb col _) _):_) prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (getIdListInCol tb col vals)
  case e of
    Left ex -> mapM_ (putFail ex) reqs
    Right a ->  mapM_ (putReq a) reqs

  where vals = [v | BlockedFetch (GetIdByCol _ _ v) _ <- reqs]
        putReq :: [(Text, Int64)] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetIdByCol {}) rvar) = putSuccess rvar Nothing
        putReq (x:xs) req@(BlockedFetch (GetIdByCol _ _ c) rvar)
          | c == fst x = putSuccess rvar (Just (snd x))
          | otherwise = putReq xs req

        putReq _ _ = return ()

fetchSync reqs@((BlockedFetch (GetCard _) _):_) prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (getCardList ids)
  case e of
    Left ex -> mapM_ (putFail ex) reqs
    Right a ->  mapM_ (putReq a) reqs

  where ids = [i | BlockedFetch (GetCard i) _ <- reqs]
        putReq :: [Card] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetCard _) rvar) = putSuccess rvar Nothing
        putReq (x:xs) req@(BlockedFetch (GetCard i) rvar)
          | i == cardID x = putSuccess rvar (Just x)
          | otherwise = putReq xs req

        putReq _ _ = return ()

fetchSync reqs prefix pool = mapM_ (\x -> fetchSync [x] prefix pool) reqs

fetchReq :: DeviceReq a -> PSQL a
fetchReq CreateTable             = createTable
fetchReq (GetDevice i)           = getDevice i
fetchReq (GetDevKeyByID k)       = getDevKeyById k

fetchReq (SaveMetric a b c d e)  = saveMetric a b c d e
fetchReq (GetMetric a)           = getMetric a
fetchReq (GetLastMetricIdList a) = getLastMetricIdList a

fetchReq (SaveIndex a b)         = saveIndex a b
fetchReq (GetIndexDevIdList a b) = getIndexDevIdList a b
fetchReq (GetIndexList a)        = getIndexList [a]

fetchReq (AddOne a b c)          = addOne a b c
fetchReq (AddOne_ a b c)         = addOne_ a b c
fetchReq (UpdateById a b c d)    = updateById a b c d
fetchReq (RemoveBy a b c)        = removeBy a b c
fetchReq (GetIdByCol a b c)      = getIdByCol a b c
fetchReq (GetIdListBy a b c d)   = getIdListBy a b c d
fetchReq (CountBy a b c)         = countBy a b c
fetchReq (GetIdListAll a b)      = getIdListAll a b
fetchReq (CountAll a)            = countAll a

fetchReq (GetCard a)             = getCard a

initDeviceState :: Int -> TablePrefix -> State DeviceReq
initDeviceState = DeviceState
