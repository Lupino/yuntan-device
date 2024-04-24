{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.Types
  ( Device (..)
  , DeviceID
  , UserName
  , Token
  , UUID
  , Meta
  , CreatedAt
  ) where

import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      object, withObject, (.:), (.=))
import           Data.Int            (Int64)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import           Database.PSQL.Types (FromRow (..), field)

type DeviceID    = Int64
type UserName    = Text
type Token       = Text
type UUID        = Text
type Meta        = Value
type CreatedAt   = Int64

data Device = Device
  { devID        :: DeviceID
  , devUserName  :: UserName
  , devToken     :: Token
  , devUUID      :: UUID
  , devMeta      :: Value
  , devPingAt    :: CreatedAt
  , devCreatedAt :: CreatedAt
  }
  deriving (Show)

instance FromRow Device where
  fromRow = do
    devID <- field
    devUserName <- field
    devToken <- field
    devUUID <- field
    devMeta <- fromMaybe Null <$> field
    devCreatedAt <- field
    return Device
      { devPingAt = 0
      , ..
      }

instance ToJSON Device where
  toJSON Device {..} = object
    [ "id"         .= devID
    , "username"   .= devUserName
    , "token"      .= devToken
    , "uuid"       .= devUUID
    , "meta"       .= devMeta
    , "ping_at"    .= devPingAt
    , "created_at" .= devCreatedAt
    ]

instance FromJSON Device where
  parseJSON = withObject "Device" $ \o -> do
    devID        <- o .: "id"
    devUserName  <- o .: "username"
    devToken     <- o .: "token"
    devUUID      <- o .: "uuid"
    devMeta      <- o .: "meta"
    devPingAt    <- o .: "ping_at"
    devCreatedAt <- o .: "created_at"
    return Device{..}
