{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.Types
  ( Device (..)
  , DeviceID
  , UserName
  , Token
  , UUID
  , Meta
  , Type
  , CreatedAt
  , TablePrefix
  ) where

import           Database.PostgreSQL.Simple (FromRow)
import           GHC.Generics               (Generic)

import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..), object, withObject,
                                             (.:), (.=))
import           Data.Int                   (Int64)
import           Data.Text                  (Text, stripEnd)

type DeviceID    = Int64
type UserName    = Text
type Token       = Text
type UUID        = Text
type Meta        = Value
type Type        = Text
type CreatedAt   = Int64
type TablePrefix = String

data Device = Device
  { devID        :: DeviceID
  , devUserName  :: UserName
  , devToken     :: Token
  , devUUID      :: UUID
  , devMeta      :: Value
  , devType      :: Type
  , devCreatedAt :: CreatedAt
  }
  deriving (Show, Generic, FromRow)

instance ToJSON Device where
  toJSON Device {..} = object
    [ "id"         .= devID
    , "username"   .= stripEnd devUserName
    , "token"      .= stripEnd devToken
    , "uuid"       .= stripEnd devUUID
    , "meta"       .= devMeta
    , "type"       .= stripEnd devType
    , "created_at" .= devCreatedAt
    ]

instance FromJSON Device where
  parseJSON = withObject "Device" $ \o -> do
    devID        <- o .: "id"
    devUserName  <- o .: "username"
    devToken     <- o .: "token"
    devUUID      <- o .: "uuid"
    devMeta      <- o .: "meta"
    devType      <- o .: "type"
    devCreatedAt <- o .: "created_at"
    return Device{..}
