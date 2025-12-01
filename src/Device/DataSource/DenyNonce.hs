{-# LANGUAGE OverloadedStrings #-}

module Device.DataSource.DenyNonce
  ( getDenyNonce
  , getDenyNonceList

  , denyNonces
  ) where


import           Database.PSQL (Columns, Only (..), PSQL, TableName, selectIn,
                                selectOne)
import           Device.Types

denyNonces :: TableName
denyNonces = "deny_nonces"

columns :: Columns
columns = ["id", "nonce", "expires_at", "created_at"]

getDenyNonce :: DenyNonceID -> PSQL (Maybe DenyNonce)
getDenyNonce mid = selectOne denyNonces columns "id = ?" (Only mid)

getDenyNonceList :: [DenyNonceID] -> PSQL [DenyNonce]
getDenyNonceList = selectIn denyNonces columns "id"
