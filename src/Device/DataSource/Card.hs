{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Card
  ( getCard
  , getCardList

  , cards
  ) where


import           Database.PSQL (Columns, Only (..), PSQL, TableName, selectIn,
                                selectOne)
import           Device.Types

cards :: TableName
cards = "cards"

columns :: Columns
columns = ["id", "dev_id", "param", "meta", "created_at"]

getCard :: CardID -> PSQL (Maybe Card)
getCard mid = selectOne cards columns "id = ?" (Only mid)

getCardList :: [CardID] -> PSQL [Card]
getCardList = selectIn cards columns "id"
