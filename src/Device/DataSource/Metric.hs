{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Metric
  ( saveMetric
  , getMetric
  , getMetricList

  , getLastMetricIdList
  , getLastMetricIdList'

  , metrics
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

getLastMetricIdList :: DeviceID -> PSQL [(String, MetricID)]
getLastMetricIdList did =
  selectAllRaw metrics ["field", "max(id)"] "dev_id = ?" (Only did) (group "field")

getLastMetricIdList' :: [DeviceID] -> PSQL [(DeviceID, String, MetricID)]
getLastMetricIdList' dids =
  selectInRaw metrics ["dev_id", "field", "max(id)"]  "dev_id" dids "" () pageNone (group "dev_id, field")
