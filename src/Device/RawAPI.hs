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
  ) where

import           Data.Int            (Int64)
import           Data.Text           (Text)
import           Database.PSQL.Types (From, HasPSQL, OrderBy, Size)
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
getDevIdByCol col val = dataFetch (GetDevIdByCol col val)

getDevIdList :: HasPSQL u => From -> Size -> OrderBy -> GenHaxl u w [DeviceID]
getDevIdList f s o = dataFetch (GetDevIdList f s o)

countDevice :: HasPSQL u => GenHaxl u w Int64
countDevice = dataFetch CountDevice

getDevIdListByKey :: HasPSQL u => KeyID -> From -> Size -> OrderBy -> GenHaxl u w [DeviceID]
getDevIdListByKey kid f s o = dataFetch (GetDevIdListByKey kid f s o)

countDeviceByKey :: HasPSQL u => KeyID -> GenHaxl u w Int64
countDeviceByKey kid = dataFetch (CountDeviceByKey kid)

updateDevice :: HasPSQL u => DeviceID -> String -> Text -> GenHaxl u w Int64
updateDevice devid f t = uncachedRequest (UpdateDevice devid f t)

removeDevice :: HasPSQL u => DeviceID -> GenHaxl u w Int64
removeDevice devid = uncachedRequest (RemoveDevice devid)

getDevKeyId :: HasPSQL u => Key -> GenHaxl u w KeyID
getDevKeyId key = dataFetch (GetDevKeyID key)

getDevKeyById :: HasPSQL u => KeyID -> GenHaxl u w Key
getDevKeyById kid = dataFetch (GetDevKeyByID kid)

getDevIdListByGw :: HasPSQL u => DeviceID -> From -> Size -> OrderBy -> GenHaxl u w [DeviceID]
getDevIdListByGw gwid f si o = dataFetch (GetDevIdListByGw gwid f si o)

countDevAddrByGw :: HasPSQL u => DeviceID -> GenHaxl u w Int64
countDevAddrByGw gwid = dataFetch (CountDevAddrByGw gwid)

saveMetric :: HasPSQL u => DeviceID -> String -> String -> Float -> CreatedAt -> GenHaxl u w Int64
saveMetric a b c d e = uncachedRequest (SaveMetric a b c d e)

getMetric :: HasPSQL u => MetricID -> GenHaxl u w (Maybe Metric)
getMetric a = dataFetch (GetMetric a)

getMetricIdList :: HasPSQL u => DeviceID -> String -> Int64 -> Int64 -> From -> Size -> OrderBy -> GenHaxl u w [MetricID]
getMetricIdList a b c d e f g = dataFetch (GetMetricIdList a b c d e f g)

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

getIndexDevIdList :: HasPSQL u => IndexNameId -> From -> Size -> OrderBy -> GenHaxl u w [DeviceID]
getIndexDevIdList a b c d = dataFetch (GetIndexDevIdList a b c d)

countIndex :: HasPSQL u => IndexNameId -> GenHaxl u w Int64
countIndex a = dataFetch (CountIndex a)
