{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Util
  ( addOne_
  , addOne
  , updateById
  , removeBy

  , getIdByCol
  , getIdListInCol

  , getIdListBy
  , countBy

  , getIdListAll
  , countAll
  ) where

import           Data.Int      (Int64)
import           Data.String   (fromString)
import           Data.Text     (Text)
import           Database.PSQL (Action, Columns, Only (..), PSQL, Page (..),
                                TableName, count, count_, delete, insertRet,
                                selectIn, selectOneOnly, selectOnly,
                                selectOnly_, toRow, update)
import           Device.Util   (getEpochTime)

addOne_ :: TableName -> Columns -> [Action] -> PSQL Int64
addOne_ tb cols vals = insertRet tb cols "id" vals 0

addOne :: TableName -> Columns -> [Action] -> PSQL Int64
addOne tb cols vals = do
  t <- getEpochTime
  addOne_ tb (cols ++ ["created_at"]) (vals ++ toRow (Only t))

updateById :: TableName -> Int64 -> Columns -> [Action] -> PSQL Int64
updateById tb byId cols vals =
  update tb cols "id = ?"  $ vals ++ toRow (Only byId)

removeBy :: TableName -> String -> [Action] -> PSQL Int64
removeBy = delete

getIdByCol :: TableName -> String -> Text -> PSQL (Maybe Int64)
getIdByCol tb col val = selectOneOnly tb "id"  (col ++ " = ?") (Only val)

getIdListInCol :: TableName -> String -> [Text] -> PSQL [(Text, Int64)]
getIdListInCol tb col' = selectIn tb [col, "id"] col
  where col = fromString col'

getIdListBy :: TableName -> String -> [Action] -> Page -> PSQL [Int64]
getIdListBy tb = selectOnly tb "id"

countBy :: TableName -> String -> [Action] -> PSQL Int64
countBy = count

getIdListAll :: TableName -> Page -> PSQL [Int64]
getIdListAll tb = selectOnly_ tb "id"

countAll :: TableName -> PSQL Int64
countAll = count_
