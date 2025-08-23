{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Util
  ( updateById
  , removeById

  , getIdByCol
  , getIdListInCol

  , getIdListByCol
  , countByCol

  , getIdList
  , countAll
  ) where

import           Data.Int            (Int64)
import           Data.String         (fromString)
import           Data.Text           (Text)
import           Database.PSQL.Types (Only (..), PSQL, Page (..), TableName,
                                      count, count_, delete, selectIn,
                                      selectOneOnly, selectOnly, selectOnly_,
                                      update)

updateById :: TableName -> Int64 -> String -> Text -> PSQL Int64
updateById tb byId col val =
  update tb [fromString col] "id = ?" (val, byId)

removeById :: TableName -> Int64 -> PSQL Int64
removeById tb byId = delete tb "id = ?" (Only byId)


getIdByCol :: TableName -> String -> Text -> PSQL (Maybe Int64)
getIdByCol tb col val = selectOneOnly tb "id"  (col ++ " = ?") (Only val)

getIdListInCol :: TableName -> String -> [Text] -> PSQL [(Text, Int64)]
getIdListInCol tb col' = selectIn tb [col, "id"] col
  where col = fromString col'

getIdListByCol :: TableName -> String -> Text -> Page -> PSQL [Int64]
getIdListByCol tb col val =
  selectOnly tb "id" (col ++ " = ?") (Only val)

countByCol :: TableName -> String -> Text -> PSQL Int64
countByCol tb col val = count tb (col ++ " = ?") (Only val)

getIdList :: TableName -> Page -> PSQL [Int64]
getIdList tb = selectOnly_ tb "id"

countAll :: TableName -> PSQL Int64
countAll = count_
