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


import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
import           Data.UnixTime
import           Database.PSQL.Types    (From (..), Only (..), OrderBy, PSQL,
                                         Size (..), TableName, count, delete,
                                         insertOrUpdate, insertRet,
                                         selectOneOnly, selectOnly)
import           Device.Types

indexNames :: TableName
indexNames = "index_names"

indexs :: TableName
indexs = "indexs"

getEpochTime :: PSQL String
getEpochTime = liftIO $ show . toEpochTime <$> getUnixTime

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

getIndexDevIdList :: IndexNameId -> From -> Size -> OrderBy -> PSQL [DeviceID]
getIndexDevIdList nid = selectOnly indexs "dev_id" "name_id = ?" (Only nid)

countIndex :: IndexNameId -> PSQL Int64
countIndex nid = count indexs "name_id = ?" (Only nid)
