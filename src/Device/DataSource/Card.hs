{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Card
  ( getCard
  , getCardList

  , cards
  ) where


import           Database.PSQL.Types (Only (..), PSQL, TableName, selectIn,
                                      selectOne)
import           Device.Types

cards :: TableName
cards = "cards"

getCard :: CardID -> PSQL (Maybe Card)
getCard mid = selectOne cards ["*"] "id = ?" (Only mid)

getCardList :: [CardID] -> PSQL [Card]
getCardList = selectIn cards ["*"] "id"
