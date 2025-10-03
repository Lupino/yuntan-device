{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Index
  ( saveIndex
  , getIndexDevIdList

  , getIndexList

  , indexs
  , indexNames
  ) where


import           Data.Int      (Int64)
import           Database.PSQL (Only (..), PSQL, Page (..), TableName, as, desc,
                                genIn, insertOrUpdate, leftJoin, pageDesc,
                                select, selectInOnly, selectOnly)
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
getIndexDevIdList [x] p  = selectOnly indexs "dev_id" "name_id = ?" (Only x) p0
  where p0 = p { pageOrder = desc "dev_id" }
getIndexDevIdList nids p = selectInOnly indexs ["DISTINCT dev_id"] "name_id" nids "" () p0
  where p0 = p { pageOrder = desc "dev_id" }


getIndexList :: [DeviceID] -> PSQL [Index]
getIndexList [] = pure []
getIndexList dids =
  select tb ["n.name", "i.dev_id", "i.created_at"] q a (pageDesc 0 0 "i.id")
  where tb = leftJoin (indexs `as` "i") (indexNames `as` "n") "n.id = i.name_id"
        (q, a) = genIn "i.dev_id" dids
