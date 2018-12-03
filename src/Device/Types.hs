{-# LANGUAGE BangPatterns      #-}
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

import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)

import           Data.Aeson                         (ToJSON (..), Value (..),
                                                     decodeStrict, object, (.=))
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)

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
  deriving (Show)

instance QueryResults Device where
  convertResults [fa, fb, fc, fd,  _, ff, fh]
                 [va, vb, vc, vd, ve, vf, vh] = Device {..}
    where !devID        = convert fa va
          !devUserName  = convert fb vb
          !devToken     = convert fc vc
          !devUUID      = convert fd vd
          !devMeta      = fromMaybe Null . decodeStrict $ fromMaybe "{}" ve
          !devType      = convert ff vf
          !devCreatedAt = convert fh vh
  convertResults fs vs  = convertError fs vs 2

instance ToJSON Device where
  toJSON Device {..} = object
    [ "id"         .= devID
    , "username"   .= devUserName
    , "token"      .= devToken
    , "uuid"       .= devUUID
    , "meta"       .= devMeta
    , "type"       .= devType
    , "created_at" .= devCreatedAt
    ]
