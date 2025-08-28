{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Metric
  ( saveMetric
  , getMetric
  , getMetricList

  , getLastMetricIdList
  , getLastMetricIdList'

  , metrics
  ) where


import           Data.Int      (Int64)
import           Database.PSQL (Only (..), PSQL, TableName, group,
                                insertOrUpdate, pageNone, selectAllRaw,
                                selectIn, selectInRaw, selectOne)
import           Device.Types

metrics :: TableName
metrics = "metrics"

saveMetric :: DeviceID -> Param -> String -> Float -> CreatedAt -> PSQL Int64
saveMetric did param rawValue value createdAt = do
  insertOrUpdate metrics uniqCols  valCols [] (did, param, createdAt, rawValue, value)

  where uniqCols = ["dev_id", "param", "created_at"]
        valCols = ["raw_value", "value"]

getMetric :: MetricID -> PSQL (Maybe Metric)
getMetric mid = selectOne metrics ["*"] "id = ?" (Only mid)

getMetricList :: [MetricID] -> PSQL [Metric]
getMetricList = selectIn metrics ["*"] "id"

getLastMetricIdList :: DeviceID -> PSQL [(Param, MetricID)]
getLastMetricIdList did =
  selectAllRaw metrics ["param", "max(id)"] "dev_id = ?" (Only did) (group "param")

getLastMetricIdList' :: [DeviceID] -> PSQL [(DeviceID, Param, MetricID)]
getLastMetricIdList' dids =
  selectInRaw metrics ["dev_id", "param", "max(id)"]  "dev_id" dids "" () pageNone (group "dev_id, param")
