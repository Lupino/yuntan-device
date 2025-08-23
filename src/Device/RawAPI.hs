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

  , updateById
  , removeBy
  , getIdByCol
  , getIdListBy
  , countBy
  ) where

import           Data.Int          (Int64)
import           Data.Text         (Text)
import           Database.PSQL     (Column, Columns, HasPSQL, Only (..), Page,
                                    TableName, ToRow (..))
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

getDevKeyById :: HasPSQL u => KeyID -> GenHaxl u w Key
getDevKeyById kid = dataFetch (GetDevKeyByID kid)

getDevIdListByGw :: HasPSQL u => DeviceID -> Page -> GenHaxl u w [DeviceID]
getDevIdListByGw gwid p =
  map DeviceID <$> getIdListBy devices "gw_id = ?" (Only gwid) p

countDevAddrByGw :: HasPSQL u => DeviceID -> GenHaxl u w Int64
countDevAddrByGw = countBy devices "gw_id = ?" . Only

saveMetric :: HasPSQL u => DeviceID -> String -> String -> Float -> CreatedAt -> GenHaxl u w Int64
saveMetric a b c d e = uncachedRequest (SaveMetric a b c d e)

getMetric :: HasPSQL u => MetricID -> GenHaxl u w (Maybe Metric)
getMetric a = dataFetch (GetMetric a)

idListSql0 = "dev_id = ? AND created_at > ?"
idListSql1 = "dev_id = ? AND field = ? AND created_at > ?"
idListSql2 = "dev_id = ? AND created_at BETWEEN ? AND ?"
idListSql3 = "dev_id = ? AND field = ? AND created_at BETWEEN ? AND ?"

getMetricIdList :: HasPSQL u => DeviceID -> String -> Int64 -> Int64 -> Page -> GenHaxl u w [MetricID]
getMetricIdList did ""    startAt 0 p =
  map MetricID <$> getIdListBy metrics idListSql0 (did, startAt) p
getMetricIdList did field startAt 0 p =
  map MetricID <$> getIdListBy metrics idListSql1 (did, field, startAt) p

getMetricIdList did ""    startAt endAt p =
  map MetricID <$> getIdListBy metrics idListSql2 (did, startAt, endAt) p
getMetricIdList did field startAt endAt p =
  map MetricID <$> getIdListBy metrics idListSql3 (did, field, startAt, endAt) p

countMetric :: HasPSQL u => DeviceID -> String -> Int64 -> Int64 -> GenHaxl u w Int64
countMetric did ""    startAt 0 = countBy metrics idListSql0 (did, startAt)
countMetric did field startAt 0 = countBy metrics idListSql1 (did, field, startAt)

countMetric did ""    startAt endAt = countBy metrics idListSql2 (did, startAt, endAt)
countMetric did field startAt endAt = countBy metrics idListSql3 (did, field, startAt, endAt)

removeMetric :: HasPSQL u => MetricID -> GenHaxl u w Int64
removeMetric = removeBy metrics "id = ?" . Only

dropMetric :: HasPSQL u => DeviceID -> String -> GenHaxl u w Int64
dropMetric did ""    = removeBy metrics "dev_id = ?" (Only did)
dropMetric did field = removeBy metrics "dev_id = ? AND field = ?" (did, field)

getLastMetricIdList :: HasPSQL u => DeviceID -> GenHaxl u w [(String, MetricID)]
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
removeIndex (Just nid) (Just did) = removeBy indexs "name_id = ? AND dev_id = ?" (nid, did)
removeIndex (Just nid) Nothing = removeBy indexs "name_id = ?" (Only nid)
removeIndex Nothing (Just did) = removeBy indexs "dev_id = ?" (Only did)
removeIndex _ _ = pure 0

getIndexDevIdList :: HasPSQL u => [IndexNameId] -> Page -> GenHaxl u w [DeviceID]
getIndexDevIdList a b = dataFetch (GetIndexDevIdList a b)

countIndex :: HasPSQL u => [IndexNameId] -> Maybe DeviceID -> GenHaxl u w Int64
countIndex a b = dataFetch (CountIndex a b)

createCard :: HasPSQL u => DeviceID -> String -> GenHaxl u w CardID
createCard did field =
  CardID <$> addOne cards ["dev_id", "field", "meta"] (did, field, "{}" :: String)

getCard :: HasPSQL u => CardID -> GenHaxl u w (Maybe Card)
getCard a = dataFetch (GetCard a)

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

countBy :: (HasPSQL u, ToRow a) => TableName -> String -> a -> GenHaxl u w Int64
countBy a b c = dataFetch (CountBy a b (toRow c))

getIdListAll :: HasPSQL u => TableName -> Page -> GenHaxl u w [Int64]
getIdListAll a b = dataFetch (GetIdListAll a b)

countAll :: HasPSQL u => TableName -> GenHaxl u w Int64
countAll a    = dataFetch (CountAll a)
