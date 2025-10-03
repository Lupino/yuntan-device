{-# LANGUAGE OverloadedStrings #-}

module Device.RawAPI
  ( createTable
  , createDevice
  , getDevice
  , getDevIdByCol
  , getDevIdList
  , getDevIdListByKey
  , countDevice
  , countDeviceByKey
  , updateDevice
  , removeDevice
  , getDevKeyId
  , getDevKeyById

  , getDevIdListByGw
  , countDevAddrByGw

  , saveMetric
  , getMetric
  , getMetricIdList
  , countMetric
  , removeMetric
  , dropMetric

  , getLastMetricIdList

  , getIndexNameId
  , getIndexNameId_
  , removeIndexName

  , saveIndex
  , removeIndex
  , getIndexDevIdList
  , countIndex
  , getIndexList

  , createCard
  , getCardId
  , getCard
  , updateCardMeta
  , removeCard
  , dropCards
  , getCards

  , updateById
  , removeBy
  , getIdByCol
  , getIdListBy
  , countBy
  ) where

import           Data.Int          (Int64)
import           Data.Maybe        (catMaybes, listToMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as T (unpack)
import           Database.PSQL     (Action, Column, Columns, HasPSQL, Only (..),
                                    Page, TableName, ToRow (..), genAnd,
                                    genBetweenBy, genBy, genEq, genIn, genMaybe,
                                    pageDesc)
import           Device.DataSource
import           Device.Types
import           Haxl.Core         (GenHaxl, dataFetch, uncachedRequest)

createTable :: HasPSQL u => GenHaxl u w Int64
createTable = uncachedRequest CreateTable

createDevice :: HasPSQL u => KeyID -> Token -> Addr -> UUID -> GenHaxl u w DeviceID
createDevice kid token addr uuid = do
  DeviceID <$> addOne devices ["key_id", "token", "uuid", "addr", "gw_id", "meta"]
    (kid, token, uuid, addr, gwid, meta)

  where meta :: String
        meta = "{}"

        gwid :: Int
        gwid = 0

getDevice :: HasPSQL u => DeviceID -> GenHaxl u w (Maybe Device)
getDevice devid = dataFetch (GetDevice devid)

getDevIdByCol :: HasPSQL u => String -> Text -> GenHaxl u w (Maybe DeviceID)
getDevIdByCol col val =
  fmap DeviceID <$> getIdByCol devices col val

getDevIdList :: HasPSQL u => Page -> GenHaxl u w [DeviceID]
getDevIdList p = map DeviceID <$> getIdListAll devices p

countDevice :: HasPSQL u => GenHaxl u w Int64
countDevice = countAll devices

getDevIdListByKey :: HasPSQL u => KeyID -> Page -> GenHaxl u w [DeviceID]
getDevIdListByKey kid p =
  map DeviceID <$> getIdListBy devices "key_id = ?" (Only kid) p

countDeviceByKey :: HasPSQL u => KeyID -> GenHaxl u w Int64
countDeviceByKey = countBy devices "key_id = ?" . Only

updateDevice :: HasPSQL u => DeviceID -> Column -> Text -> GenHaxl u w Int64
updateDevice (DeviceID did) f t = updateById devices did [f] (Only t)

removeDevice :: HasPSQL u => DeviceID -> GenHaxl u w Int64
removeDevice = removeBy devices "id = ?" . Only

getDevKeyId :: HasPSQL u => Key -> GenHaxl u w KeyID
getDevKeyId (Key key) = do
  mkid <- getIdByCol deviceKeys "devkey" key
  case mkid of
    Nothing  -> KeyID <$> addOne deviceKeys ["devkey"] (Only key)
    Just kid -> pure $ KeyID kid

getDevKeyById :: HasPSQL u => KeyID -> GenHaxl u w (Maybe Key)
getDevKeyById kid = dataFetch (GetDevKeyByID kid)

getDevIdListByGw :: HasPSQL u => DeviceID -> Page -> GenHaxl u w [DeviceID]
getDevIdListByGw gwid p =
  map DeviceID <$> getIdListBy devices "gw_id = ?" (Only gwid) p

countDevAddrByGw :: HasPSQL u => DeviceID -> GenHaxl u w Int64
countDevAddrByGw = countBy devices "gw_id = ?" . Only

saveMetric :: HasPSQL u => DeviceID -> Param -> String -> Float -> CreatedAt -> GenHaxl u w Int64
saveMetric a b c d e = uncachedRequest (SaveMetric a b c d e)

getMetric :: HasPSQL u => MetricID -> GenHaxl u w (Maybe Metric)
getMetric a = dataFetch (GetMetric a)


genParamQuery :: Param -> (String, [Action])
genParamQuery (Param param) = genBy (not . null) "param" $ T.unpack param

genGetMetricIdListQuery :: DeviceID -> Param -> Int64 -> Int64 -> (String, [Action])
genGetMetricIdListQuery did param startAt endAt = (q, a)
  where (q0, a0) = genEq "dev_id" did
        (q1, a1) = genParamQuery param
        (q2, a2) = genBetweenBy (> 0) "created_at" startAt endAt

        q = q0 `genAnd` q1 `genAnd` q2
        a = a0 ++ a1 ++ a2


getMetricIdList :: HasPSQL u => DeviceID -> Param -> Int64 -> Int64 -> Page -> GenHaxl u w [MetricID]
getMetricIdList did param    startAt endAt p =
  map MetricID <$> getIdListBy metrics sql args p
  where (sql, args) = genGetMetricIdListQuery did param startAt endAt

countMetric :: HasPSQL u => DeviceID -> Param -> Int64 -> Int64 -> GenHaxl u w Int64
countMetric did param    startAt endAt = countBy metrics sql args
  where (sql, args) = genGetMetricIdListQuery did param startAt endAt

removeMetric :: HasPSQL u => MetricID -> GenHaxl u w Int64
removeMetric = removeBy metrics "id = ?" . Only

dropMetric :: HasPSQL u => DeviceID -> Param -> GenHaxl u w Int64
dropMetric did param = removeBy metrics q a
  where (q0, a0) = genEq "dev_id" did
        (q1, a1) = genParamQuery param

        q = q0 `genAnd` q1
        a = a0 ++ a1

getLastMetricIdList :: HasPSQL u => DeviceID -> GenHaxl u w [(Param, MetricID)]
getLastMetricIdList a = dataFetch (GetLastMetricIdList a)

createIndexName :: HasPSQL u => IndexName -> GenHaxl u w IndexNameId
createIndexName name = IndexNameId <$> addOne indexNames ["name"] (Only name)

getIndexNameId :: HasPSQL u => IndexName -> GenHaxl u w IndexNameId
getIndexNameId name = do
  mnid <- getIndexNameId_ name
  case mnid of
    Nothing  -> createIndexName name
    Just nid -> pure nid

getIndexNameId_ :: HasPSQL u => IndexName -> GenHaxl u w (Maybe IndexNameId)
getIndexNameId_ (IndexName n) =
  fmap IndexNameId <$> getIdByCol indexNames "name" n

removeIndexName :: HasPSQL u => IndexNameId -> GenHaxl u w Int64
removeIndexName nid = removeBy indexNames "id = ?" (Only nid)

saveIndex :: HasPSQL u => IndexNameId -> DeviceID -> GenHaxl u w Int64
saveIndex a b = uncachedRequest (SaveIndex a b)

removeIndex :: HasPSQL u => Maybe IndexNameId -> Maybe DeviceID -> GenHaxl u w Int64
removeIndex Nothing Nothing = pure 0
removeIndex mNid mDid = removeBy indexs q a
  where (q0, a0) = genMaybe "name_id" mNid
        (q1, a1) = genMaybe "dev_id" mDid
        q = q0 `genAnd` q1
        a = a0 ++ a1

getIndexDevIdList :: HasPSQL u => [IndexNameId] -> Page -> GenHaxl u w [DeviceID]
getIndexDevIdList a b = dataFetch (GetIndexDevIdList a b)

getIndexList :: HasPSQL u => DeviceID -> GenHaxl u w [Index]
getIndexList = dataFetch . GetIndexList

countIndex :: HasPSQL u => [IndexNameId] -> Maybe DeviceID -> GenHaxl u w Int64
countIndex [] Nothing   = pure 0
countIndex nids mDid = countColBy indexs "DISTINCT dev_id" q a
  where (q0, a0) = genMaybe "dev_id" mDid
        (q1, a1) = genIn "name_id" nids

        q = q0 `genAnd` q1
        a = a0 ++ a1

createCard :: HasPSQL u => DeviceID -> Param -> Meta -> GenHaxl u w CardID
createCard did param meta =
  CardID <$> addOne cards ["dev_id", "param", "meta"] (did, param, meta)

updateCardMeta :: HasPSQL u => CardID -> Meta -> GenHaxl u w Int64
updateCardMeta (CardID cid) meta =
  updateById cards cid ["meta"] (Only meta)

getCardId :: HasPSQL u => DeviceID -> Param -> GenHaxl u w (Maybe CardID)
getCardId did param =
  fmap CardID <$> getIdBy cards "dev_id = ? AND param = ?" (did, param)

getCard :: HasPSQL u => CardID -> GenHaxl u w (Maybe Card)
getCard a = dataFetch (GetCard a)

removeCard :: HasPSQL u => CardID -> GenHaxl u w Int64
removeCard = removeBy cards "id = ?" . Only

dropCards :: HasPSQL u => DeviceID -> GenHaxl u w Int64
dropCards = removeBy cards "dev_id = ?" . Only

getCards :: HasPSQL u => DeviceID -> GenHaxl u w [Card]
getCards did = do
  cardIds <- getIdListBy cards "dev_id = ?" (Only did) (pageDesc 0 0 "id")
  catMaybes <$> mapM (getCard . CardID) cardIds

---------------------------- Util ----------------------------------

-- no created_at
addOne_ :: (HasPSQL u, ToRow a) => TableName -> Columns -> a -> GenHaxl u w Int64
addOne_ a b c = uncachedRequest (AddOne_ a b (toRow c))

-- auto append created_at
addOne :: (HasPSQL u, ToRow a) => TableName -> Columns -> a -> GenHaxl u w Int64
addOne a b c = uncachedRequest (AddOne a b (toRow c))

updateById :: (HasPSQL u, ToRow a) => TableName -> Int64 -> Columns -> a -> GenHaxl u w Int64
updateById a b c d = uncachedRequest (UpdateById a b c (toRow d))

removeBy :: (HasPSQL u, ToRow a) => TableName -> String -> a -> GenHaxl u w Int64
removeBy a b c = uncachedRequest (RemoveBy a b (toRow c))

getIdByCol :: HasPSQL u => TableName -> String -> Text -> GenHaxl u w (Maybe Int64)
getIdByCol a b c = dataFetch (GetIdByCol a b c)

getIdListBy :: (HasPSQL u, ToRow a) => TableName -> String -> a -> Page -> GenHaxl u w [Int64]
getIdListBy a b c p = dataFetch (GetIdListBy a b (toRow c) p)

getIdBy :: (HasPSQL u, ToRow a) => TableName -> String -> a -> GenHaxl u w (Maybe Int64)
getIdBy a b c = listToMaybe <$> getIdListBy a b c (pageDesc 0 1 "id")

countBy :: (HasPSQL u, ToRow a) => TableName -> String -> a -> GenHaxl u w Int64
countBy a b c = dataFetch (CountBy a b (toRow c))

countColBy :: (HasPSQL u, ToRow a) => TableName -> Column -> String -> a -> GenHaxl u w Int64
countColBy a b c d = dataFetch (CountColBy a b c (toRow d))

getIdListAll :: HasPSQL u => TableName -> Page -> GenHaxl u w [Int64]
getIdListAll a b = dataFetch (GetIdListAll a b)

countAll :: HasPSQL u => TableName -> GenHaxl u w Int64
countAll a    = dataFetch (CountAll a)
