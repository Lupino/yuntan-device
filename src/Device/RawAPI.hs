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
  , removeById
  , getIdByCol
  , getIdListByCol
  , countByCol

  , devices
  ) where

import           Data.Int            (Int64)
import           Data.String         (fromString)
import           Data.Text           (Text)
import           Database.PSQL.Types (HasPSQL, Page, TableName)
import           Device.DataSource
import           Device.Types
import           Haxl.Core           (GenHaxl, dataFetch, uncachedRequest)

createTable :: HasPSQL u => GenHaxl u w Int64
createTable = uncachedRequest CreateTable

createDevice :: HasPSQL u => KeyID -> Token -> Addr ->  GenHaxl u w DeviceID
createDevice kid token addr = uncachedRequest (CreateDevice kid token addr)

getDevice :: HasPSQL u => DeviceID -> GenHaxl u w (Maybe Device)
getDevice devid = dataFetch (GetDevice devid)

getDevIdByCol :: HasPSQL u => String -> Text -> GenHaxl u w (Maybe DeviceID)
getDevIdByCol col val =
  maybe Nothing (Just . DeviceID) <$> getIdByCol devices col val

getDevIdList :: HasPSQL u => Page -> GenHaxl u w [DeviceID]
getDevIdList p = map DeviceID <$> getIdList devices p

countDevice :: HasPSQL u => GenHaxl u w Int64
countDevice = countAll devices

getDevIdListByKey :: HasPSQL u => KeyID -> Page -> GenHaxl u w [DeviceID]
getDevIdListByKey (KeyID kid) p =
  map DeviceID <$> getIdListByCol devices "key_id" k p
  where k = fromString (show kid)

countDeviceByKey :: HasPSQL u => KeyID -> GenHaxl u w Int64
countDeviceByKey (KeyID kid) = countByCol devices "key_id" k
  where k = fromString (show kid)

updateDevice :: HasPSQL u => DeviceID -> String -> Text -> GenHaxl u w Int64
updateDevice (DeviceID did) f t = updateById devices did f t

removeDevice :: HasPSQL u => DeviceID -> GenHaxl u w Int64
removeDevice (DeviceID did) = removeById devices did

getDevKeyId :: HasPSQL u => Key -> GenHaxl u w KeyID
getDevKeyId key = dataFetch (GetDevKeyID key)

getDevKeyById :: HasPSQL u => KeyID -> GenHaxl u w Key
getDevKeyById kid = dataFetch (GetDevKeyByID kid)

getDevIdListByGw :: HasPSQL u => DeviceID -> Page -> GenHaxl u w [DeviceID]
getDevIdListByGw (DeviceID gwid) p =
  map DeviceID <$> getIdListByCol devices "gw_id" k p
  where k = fromString (show gwid)

countDevAddrByGw :: HasPSQL u => DeviceID -> GenHaxl u w Int64
countDevAddrByGw (DeviceID gwid) = countByCol devices "gw_id" k
  where k = fromString (show gwid)

saveMetric :: HasPSQL u => DeviceID -> String -> String -> Float -> CreatedAt -> GenHaxl u w Int64
saveMetric a b c d e = uncachedRequest (SaveMetric a b c d e)

getMetric :: HasPSQL u => MetricID -> GenHaxl u w (Maybe Metric)
getMetric a = dataFetch (GetMetric a)

getMetricIdList :: HasPSQL u => DeviceID -> String -> Int64 -> Int64 -> Page -> GenHaxl u w [MetricID]
getMetricIdList a b c d e = dataFetch (GetMetricIdList a b c d e)

countMetric :: HasPSQL u => DeviceID -> String -> Int64 -> Int64 -> GenHaxl u w Int64
countMetric a b c d = dataFetch (CountMetric a b c d)

removeMetric :: HasPSQL u => MetricID -> GenHaxl u w Int64
removeMetric a = uncachedRequest (RemoveMetric a)

dropMetric :: HasPSQL u => DeviceID -> String -> GenHaxl u w Int64
dropMetric a f = uncachedRequest (DropMetric a f)

getLastMetricIdList :: HasPSQL u => DeviceID -> GenHaxl u w [(String, MetricID)]
getLastMetricIdList a = dataFetch (GetLastMetricIdList a)

getIndexNameId :: HasPSQL u => IndexName -> GenHaxl u w IndexNameId
getIndexNameId a = dataFetch (GetIndexNameId a)

getIndexNameId_ :: HasPSQL u => IndexName -> GenHaxl u w (Maybe IndexNameId)
getIndexNameId_ a = dataFetch (GetIndexNameId_ a)

removeIndexName :: HasPSQL u => IndexNameId -> GenHaxl u w Int64
removeIndexName a = uncachedRequest (RemoveIndexName a)

saveIndex :: HasPSQL u => IndexNameId -> DeviceID -> GenHaxl u w Int64
saveIndex a b = uncachedRequest (SaveIndex a b)

removeIndex :: HasPSQL u => Maybe IndexNameId -> Maybe DeviceID -> GenHaxl u w Int64
removeIndex a b = uncachedRequest (RemoveIndex a b)

getIndexDevIdList :: HasPSQL u => [IndexNameId] -> Page -> GenHaxl u w [DeviceID]
getIndexDevIdList a b = dataFetch (GetIndexDevIdList a b)

countIndex :: HasPSQL u => [IndexNameId] -> Maybe DeviceID -> GenHaxl u w Int64
countIndex a b = dataFetch (CountIndex a b)

---------------------------- Util ----------------------------------

updateById :: HasPSQL u => TableName -> Int64 -> String -> Text -> GenHaxl u w Int64
updateById a b c d = uncachedRequest (UpdateById a b c d)

removeById :: HasPSQL u => TableName -> Int64 -> GenHaxl u w Int64
removeById a b = uncachedRequest (RemoveById a b)

getIdByCol :: HasPSQL u => TableName -> String -> Text -> GenHaxl u w (Maybe Int64)
getIdByCol a b c = dataFetch (GetIdByCol a b c)

getIdListInCol :: HasPSQL u => TableName -> String -> [Text] -> GenHaxl u w [(Text, Int64)]
getIdListInCol a b c = dataFetch (GetIdListInCol a b c)

getIdListByCol :: HasPSQL u => TableName -> String -> Text -> Page -> GenHaxl u w [Int64]
getIdListByCol a b c d = dataFetch (GetIdListByCol a b c d)

countByCol :: HasPSQL u => TableName -> String -> Text -> GenHaxl u w Int64
countByCol a b c = dataFetch (CountByCol a b c)

getIdList :: HasPSQL u => TableName -> Page -> GenHaxl u w [Int64]
getIdList a b = dataFetch (GetIdList a b)

countAll :: HasPSQL u => TableName -> GenHaxl u w Int64
countAll a    = dataFetch (CountAll a)
