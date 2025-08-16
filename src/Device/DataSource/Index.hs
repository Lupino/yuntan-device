{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Index
  ( getIndexNameId
  , getIndexNameId_
  , removeIndexName

  , saveIndex
  , removeIndex
  , getIndexDevIdList
  , countIndex
  ) where


import           Data.Int            (Int64)
import           Database.PSQL.Types (Only (..), PSQL, Page (..), TableName,
                                      count, countInRaw, delete, insertOrUpdate,
                                      insertRet, selectInOnly, selectOneOnly,
                                      selectOnly)
import           Device.Types
import           Device.Util         (getEpochTime)

indexNames :: TableName
indexNames = "index_names"

indexs :: TableName
indexs = "indexs"

createIndexName :: IndexName -> PSQL IndexNameId
createIndexName name = do
  t <- getEpochTime
  insertRet indexNames ["name", "created_at"] "id" (name, t) 0


removeIndexName :: IndexNameId -> PSQL Int64
removeIndexName nid = delete indexNames "id = ?" (Only nid)


getIndexNameId_ :: IndexName -> PSQL (Maybe IndexNameId)
getIndexNameId_ name = selectOneOnly indexNames "id" "name = ?" (Only name)

getIndexNameId :: IndexName -> PSQL IndexNameId
getIndexNameId name = do
  mnid <- getIndexNameId_ name
  case mnid of
    Nothing  -> createIndexName name
    Just nid -> pure nid

saveIndex :: IndexNameId -> DeviceID -> PSQL Int64
saveIndex nid did = do
  createdAt <- getEpochTime
  insertOrUpdate indexs uniqCols [] otherCols (nid, did, createdAt)

  where uniqCols = ["name_id", "dev_id"]
        otherCols = ["created_at"]

removeIndex :: Maybe IndexNameId -> Maybe DeviceID -> PSQL Int64
removeIndex (Just nid) (Just did) = delete indexs "name_id = ? AND dev_id = ?" (nid, did)
removeIndex (Just nid) Nothing = delete indexs "name_id = ?" (Only nid)
removeIndex Nothing (Just did) = delete indexs "dev_id = ?" (Only did)
removeIndex _ _ = pure 0

getIndexDevIdList :: [IndexNameId] -> Page -> PSQL [DeviceID]
getIndexDevIdList [] _   = pure []
getIndexDevIdList [x] p  = selectOnly indexs "dev_id" "name_id = ?" (Only x) p
getIndexDevIdList nids p = selectInOnly indexs ["dev_id"] "name_id" nids "" () p

countIndex :: [IndexNameId] -> Maybe DeviceID -> PSQL Int64
countIndex [] Nothing       = pure 0
countIndex [nid] Nothing    = count indexs "name_id                    = ?" (Only nid)
countIndex [nid] (Just did) = count indexs "name_id                    = ? AND dev_id  = ?" (nid, did)
countIndex nids Nothing     = countInRaw indexs "name_id" nids "" ()
countIndex nids (Just did)  = countInRaw indexs "name_id" nids "dev_id = ?" (Only did)
