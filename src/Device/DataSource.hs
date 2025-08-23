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
  , cards
  ) where


import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception        as CE (SomeException, bracket_, try)
import           Data.Hashable            (Hashable (..))
import           Data.Int                 (Int64)
import           Data.List                (groupBy)
import           Data.Text                (Text)
import           Data.Typeable            (Typeable)
import           Database.PSQL.Types      (HasPSQL, PSQL, PSQLPool, Page,
                                           TableName, TablePrefix, psqlPool,
                                           runPSQLPool)
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
  CreateDevice :: KeyID -> Token -> Addr -> DeviceReq DeviceID
  GetDevice :: DeviceID -> DeviceReq (Maybe Device)
  GetDevKeyID :: Key -> DeviceReq KeyID
  GetDevKeyByID :: KeyID -> DeviceReq Key

  SaveMetric :: DeviceID -> String -> String -> Float -> CreatedAt -> DeviceReq Int64
  GetMetric :: MetricID -> DeviceReq (Maybe Metric)
  GetMetricIdList :: DeviceID -> String -> Int64 -> Int64 -> Page -> DeviceReq [MetricID]
  CountMetric :: DeviceID -> String -> Int64 -> Int64 -> DeviceReq Int64
  RemoveMetric :: MetricID -> DeviceReq Int64
  DropMetric :: DeviceID -> String -> DeviceReq Int64
  GetLastMetricIdList :: DeviceID -> DeviceReq [(String, MetricID)]

  GetIndexNameId :: IndexName -> DeviceReq IndexNameId
  GetIndexNameId_ :: IndexName -> DeviceReq (Maybe IndexNameId)
  RemoveIndexName :: IndexNameId -> DeviceReq Int64

  SaveIndex :: IndexNameId -> DeviceID -> DeviceReq Int64
  RemoveIndex :: Maybe IndexNameId -> Maybe DeviceID -> DeviceReq Int64
  GetIndexDevIdList :: [IndexNameId] -> Page -> DeviceReq [DeviceID]
  CountIndex :: [IndexNameId] -> Maybe DeviceID -> DeviceReq Int64

  UpdateById :: TableName -> Int64 -> String -> Text -> DeviceReq Int64
  RemoveById :: TableName -> Int64 -> DeviceReq Int64
  GetIdByCol :: TableName -> String -> Text -> DeviceReq (Maybe Int64)
  GetIdListInCol :: TableName -> String -> [Text] -> DeviceReq [(Text, Int64)]
  GetIdListByCol :: TableName -> String -> Text -> Page -> DeviceReq [Int64]
  CountByCol :: TableName -> String -> Text -> DeviceReq Int64
  GetIdList :: TableName -> Page -> DeviceReq [Int64]
  CountAll :: TableName -> DeviceReq Int64

  CreateCard :: DeviceID -> String -> DeviceReq CardID
  GetCard :: CardID -> DeviceReq (Maybe Card)

  deriving (Typeable)

deriving instance Eq (DeviceReq a)
instance Hashable (DeviceReq a) where
  hashWithSalt s CreateTable                 = hashWithSalt s (1::Int)
  hashWithSalt s (CreateDevice k t a)        = hashWithSalt s (2::Int, k, t, a)
  hashWithSalt s (GetDevice i)               = hashWithSalt s (3::Int, i)
  hashWithSalt s (GetDevKeyID k)             = hashWithSalt s (4::Int, k)
  hashWithSalt s (GetDevKeyByID k)           = hashWithSalt s (5::Int, k)

  hashWithSalt s (SaveMetric a b c d e)      = hashWithSalt s (6::Int, a, b, c, d, e)
  hashWithSalt s (GetMetric a)               = hashWithSalt s (7::Int, a)
  hashWithSalt s (GetMetricIdList a b c d p) = hashWithSalt s (8::Int, a, b, c, d, p)
  hashWithSalt s (CountMetric a b c d)       = hashWithSalt s (9::Int, a, b, c, d)
  hashWithSalt s (RemoveMetric a)            = hashWithSalt s (10::Int, a)
  hashWithSalt s (DropMetric a f)            = hashWithSalt s (11::Int, a, f)
  hashWithSalt s (GetLastMetricIdList a)     = hashWithSalt s (12::Int, a)

  hashWithSalt s (GetIndexNameId a)          = hashWithSalt s (13::Int, a)
  hashWithSalt s (GetIndexNameId_ a)         = hashWithSalt s (14::Int, a)
  hashWithSalt s (RemoveIndexName a)         = hashWithSalt s (15::Int, a)

  hashWithSalt s (SaveIndex a b)             = hashWithSalt s (16::Int, a, b)
  hashWithSalt s (RemoveIndex a b)           = hashWithSalt s (17::Int, a, b)
  hashWithSalt s (GetIndexDevIdList a b)     = hashWithSalt s (18::Int, a, b)
  hashWithSalt s (CountIndex a b)            = hashWithSalt s (19::Int, a, b)

  hashWithSalt s (UpdateById a b c d)        = hashWithSalt s (20::Int, a, b, c, d)
  hashWithSalt s (RemoveById a b)            = hashWithSalt s (21::Int, a, b)
  hashWithSalt s (GetIdByCol a b c)          = hashWithSalt s (22::Int, a, b, c)
  hashWithSalt s (GetIdListInCol a b c)      = hashWithSalt s (23::Int, a, b, c)
  hashWithSalt s (GetIdListByCol a b c d)    = hashWithSalt s (24::Int, a, b, c, d)
  hashWithSalt s (CountByCol a b c)          = hashWithSalt s (25::Int, a, b, c)
  hashWithSalt s (GetIdList a b)             = hashWithSalt s (26::Int, a, b)
  hashWithSalt s (CountAll a)                = hashWithSalt s (27::Int64, a)

  hashWithSalt s (CreateCard a b)            = hashWithSalt s (28::Int64, a, b)
  hashWithSalt s (GetCard a)                 = hashWithSalt s (29::Int64, a)

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
        putReq [] (BlockedFetch (GetDevKeyByID _) rvar) = putSuccess rvar ""
        putReq (x:xs) req@(BlockedFetch (GetDevKeyByID i) rvar)
          | i == fst x = putSuccess rvar (snd x)
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

fetchSync reqs@((BlockedFetch (GetIdByCol tb col _) _):_) prefix pool = do
  e <- CE.try $ runPSQLPool prefix pool (getIdListInCol tb col vals)
  case e of
    Left ex -> mapM_ (putFail ex) reqs
    Right a ->  mapM_ (putReq a) reqs

  where vals = [v | BlockedFetch (GetIdByCol _ _ v) _ <- reqs]
        putReq :: [(Text, Int64)] -> BlockedFetch DeviceReq ->  IO ()
        putReq [] (BlockedFetch (GetIdByCol _ _ _) rvar) = putSuccess rvar Nothing
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
fetchReq CreateTable                 = createTable
fetchReq (CreateDevice k t a)        = createDevice k t a
fetchReq (GetDevice i)               = getDevice i
fetchReq (GetDevKeyID k)             = getDevKeyId k
fetchReq (GetDevKeyByID k)           = getDevKeyById k

fetchReq (SaveMetric a b c d e)      = saveMetric a b c d e
fetchReq (GetMetric a)               = getMetric a
fetchReq (GetMetricIdList a b c d e) = getMetricIdList a b c d e
fetchReq (CountMetric a b c d)       = countMetric a b c d
fetchReq (RemoveMetric a)            = removeMetric a
fetchReq (DropMetric a f)            = dropMetric a f
fetchReq (GetLastMetricIdList a)     = getLastMetricIdList a

fetchReq (GetIndexNameId a)          = getIndexNameId a
fetchReq (GetIndexNameId_ a)         = getIndexNameId_ a
fetchReq (RemoveIndexName a)         = removeIndexName a

fetchReq (SaveIndex a b)             = saveIndex a b
fetchReq (RemoveIndex a b)           = removeIndex a b
fetchReq (GetIndexDevIdList a b)     = getIndexDevIdList a b
fetchReq (CountIndex a b)            = countIndex a b

fetchReq (UpdateById a b c d)        = updateById a b c d
fetchReq (RemoveById a b)            = removeById a b
fetchReq (GetIdByCol a b c)          = getIdByCol a b c
fetchReq (GetIdListInCol a b c)      = getIdListInCol a b c
fetchReq (GetIdListByCol a b c d)    = getIdListByCol a b c d
fetchReq (CountByCol a b c)          = countByCol a b c
fetchReq (GetIdList a b)             = getIdList a b
fetchReq (CountAll a)                = countAll a

fetchReq (CreateCard a b)            = createCard a b
fetchReq (GetCard a)                 = getCard a

initDeviceState :: Int -> TablePrefix -> State DeviceReq
initDeviceState = DeviceState
