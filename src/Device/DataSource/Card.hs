{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.Card
  ( createCard
  , getCard
  , getCardList

  , cards
  ) where


import           Database.PSQL.Types (Only (..), PSQL, TableName, insertRet,
                                      selectIn, selectOne)
import           Device.Types
import           Device.Util         (getEpochTime)

cards :: TableName
cards = "cards"

createCard :: DeviceID -> String -> PSQL CardID
createCard did field = do
  t <- getEpochTime
  insertRet cards ["dev_id", "field", "meta", "created_at"] "id" (did, field, "{}" :: String, t) 0

getCard :: CardID -> PSQL (Maybe Card)
getCard mid = selectOne cards ["*"] "id = ?" (Only mid)

getCardList :: [CardID] -> PSQL [Card]
getCardList = selectIn cards ["*"] "id"
