{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Index
  ( saveIndex
  , getIndexDevIdList

  , indexs
  , indexNames
  ) where


import           Data.Int      (Int64)
import           Database.PSQL (Only (..), PSQL, Page (..), TableName,
                                insertOrUpdate, selectInOnly, selectOnly)
import           Device.Types
import           Device.Util   (getEpochTime)

indexNames :: TableName
indexNames = "index_names"

indexs :: TableName
indexs = "indexs"

saveIndex :: IndexNameId -> DeviceID -> PSQL Int64
saveIndex nid did = do
  createdAt <- getEpochTime
  insertOrUpdate indexs uniqCols [] otherCols (nid, did, createdAt)

  where uniqCols = ["name_id", "dev_id"]
        otherCols = ["created_at"]

getIndexDevIdList :: [IndexNameId] -> Page -> PSQL [DeviceID]
getIndexDevIdList [] _   = pure []
getIndexDevIdList [x] p  = selectOnly indexs "dev_id" "name_id = ?" (Only x) p
getIndexDevIdList nids p = selectInOnly indexs ["dev_id"] "name_id" nids "" () p
