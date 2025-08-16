{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Metric
  ( saveMetric
  , getMetric
  , getMetricList
  , getMetricIdList
  , countMetric
  , removeMetric
  , dropMetric

  , getLastMetricIdList
  , getLastMetricIdList'
  ) where


import           Data.Int            (Int64)
import           Database.PSQL.Types (Only (..), PSQL, Page (..), TableName,
                                      count, delete, group, insertOrUpdate,
                                      pageNone, selectAllRaw, selectIn,
                                      selectInRaw, selectOne, selectOnly)
import           Device.Types

metrics :: TableName
metrics = "metrics"

saveMetric :: DeviceID -> String -> String -> Float -> CreatedAt -> PSQL Int64
saveMetric did field rawValue value createdAt = do
  insertOrUpdate metrics uniqCols  valCols [] (did, field, createdAt, rawValue, value)

  where uniqCols = ["dev_id", "field", "created_at"]
        valCols = ["raw_value", "value"]

getMetric :: MetricID -> PSQL (Maybe Metric)
getMetric mid = selectOne metrics ["*"] "id = ?" (Only mid)

getMetricList :: [MetricID] -> PSQL [Metric]
getMetricList = selectIn metrics ["*"] "id"

idListSql0 = "dev_id = ? AND created_at > ?"
idListSql1 = "dev_id = ? AND field = ? AND created_at > ?"
idListSql2 = "dev_id = ? AND created_at BETWEEN ? AND ?"
idListSql3 = "dev_id = ? AND field = ? AND created_at BETWEEN ? AND ?"

getMetricIdList :: DeviceID -> String -> Int64 -> Int64 -> Page -> PSQL [MetricID]
getMetricIdList did ""    startAt 0 = selectOnly metrics "id" idListSql0 (did, startAt)
getMetricIdList did field startAt 0 = selectOnly metrics "id" idListSql1 (did, field, startAt)

getMetricIdList did ""    startAt endAt = selectOnly metrics "id" idListSql2 (did, startAt, endAt)
getMetricIdList did field startAt endAt = selectOnly metrics "id" idListSql3 (did, field, startAt, endAt)

countMetric :: DeviceID -> String -> Int64 -> Int64 -> PSQL Int64
countMetric did ""    startAt 0 = count metrics idListSql0 (did, startAt)
countMetric did field startAt 0 = count metrics idListSql1 (did, field, startAt)

countMetric did ""    startAt endAt = count metrics idListSql2 (did, startAt, endAt)
countMetric did field startAt endAt = count metrics idListSql3 (did, field, startAt, endAt)

removeMetric :: MetricID -> PSQL Int64
removeMetric mid = delete metrics "id = ?" (Only mid)

dropMetric :: DeviceID -> String -> PSQL Int64
dropMetric did ""    = delete metrics "dev_id = ?" (Only did)
dropMetric did field = delete metrics "dev_id = ? AND field = ?" (did, field)

getLastMetricIdList :: DeviceID -> PSQL [(String, MetricID)]
getLastMetricIdList did =
  selectAllRaw metrics ["field", "max(id)"] "dev_id = ?" (Only did) (group "field")

getLastMetricIdList' :: [DeviceID] -> PSQL [(DeviceID, String, MetricID)]
getLastMetricIdList' dids =
  selectInRaw metrics ["dev_id", "field", "max(id)"]  "dev_id" dids "" () pageNone (group "dev_id, field")
