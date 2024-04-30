{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.Types
  ( Device (..)
  , DeviceID (..)
  , Key (..)
  , KeyID (..)
  , Token (..)
  , UUID (..)
  , Meta
  , CreatedAt (..)
  , Addr (..)
  ) where

import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      object, withObject, withScientific,
                                      withText, (.:), (.=))
import           Data.Hashable       (Hashable (..))
import           Data.Int            (Int64)
import           Data.Maybe          (fromMaybe)
import           Data.Scientific     (toBoundedInteger)
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import           Database.PSQL.Types (FromField (..), FromRow (..),
                                      ToField (..), field)


data Key = Key {unKey :: Text}
  deriving (Show, Eq, Ord)

instance Hashable Key where
  hashWithSalt s (Key v) = hashWithSalt s v


instance ToField Key where
  toField (Key k) = toField k

instance FromField Key where
  fromField f mv = Key <$> fromField f mv

instance IsString Key where
  fromString = Key . fromString

instance FromJSON Key where
  parseJSON = withText "Key" $ \t -> pure $ Key t

instance ToJSON Key where
  toJSON (Key k) = toJSON k


data Addr = Addr {unAddr :: Text}
  deriving (Show, Eq, Ord)

instance Hashable Addr where
  hashWithSalt s (Addr v) = hashWithSalt s v


instance ToField Addr where
  toField (Addr k) = toField k

instance FromField Addr where
  fromField f mv = Addr <$> fromField f mv

instance IsString Addr where
  fromString = Addr . fromString

instance FromJSON Addr where
  parseJSON = withText "Addr" $ \t -> pure $ Addr t

instance ToJSON Addr where
  toJSON (Addr k) = toJSON k

data Token = Token {unToken :: Text}
  deriving (Show, Eq, Ord)

instance Hashable Token where
  hashWithSalt s (Token v) = hashWithSalt s v


instance ToField Token where
  toField (Token k) = toField k

instance FromField Token where
  fromField f mv = Token <$> fromField f mv

instance IsString Token where
  fromString = Token . fromString

instance FromJSON Token where
  parseJSON = withText "Token" $ \t -> pure $ Token t

instance ToJSON Token where
  toJSON (Token k) = toJSON k

data UUID = UUID {unUUID :: Text}
  deriving (Show, Eq, Ord)

instance Hashable UUID where
  hashWithSalt s (UUID v) = hashWithSalt s v

instance ToField UUID where
  toField (UUID k) = toField k

instance FromField UUID where
  fromField f mv = UUID <$> fromField f mv

instance IsString UUID where
  fromString = UUID . fromString

instance FromJSON UUID where
  parseJSON = withText "UUID" $ \t -> pure $ UUID t

instance ToJSON UUID where
  toJSON (UUID k) = toJSON k


data CreatedAt = CreatedAt {unCreatedAt :: Int64}
  deriving (Show, Eq, Ord)

instance Hashable CreatedAt where
  hashWithSalt s (CreatedAt v) = hashWithSalt s v

instance ToField CreatedAt where
  toField (CreatedAt k) = toField k

instance FromField CreatedAt where
  fromField f mv = CreatedAt <$> fromField f mv

instance Num CreatedAt where
  CreatedAt c1 + CreatedAt c2 = CreatedAt (c1 + c2)
  {-# INLINABLE (+) #-}

  CreatedAt c1 - CreatedAt c2 = CreatedAt (c1 - c2)
  {-# INLINABLE (-) #-}

  CreatedAt c1 * CreatedAt c2 = CreatedAt (c1 * c2)
  {-# INLINABLE (*) #-}

  abs (CreatedAt c) = CreatedAt (abs c)
  {-# INLINABLE abs #-}

  negate (CreatedAt c) = CreatedAt (negate c)
  {-# INLINABLE negate #-}

  signum (CreatedAt c) = CreatedAt (signum c)
  {-# INLINABLE signum #-}

  fromInteger = CreatedAt . fromInteger
  {-# INLINABLE fromInteger #-}

instance FromJSON CreatedAt where
  parseJSON = withScientific "CreatedAt" $ \t ->
    pure $ CreatedAt $ fromMaybe 0 $ toBoundedInteger t

instance ToJSON CreatedAt where
  toJSON (CreatedAt k) = toJSON k


data KeyID = KeyID {unKeyID :: Int64}
  deriving (Show, Eq, Ord)

instance Hashable KeyID where
  hashWithSalt s (KeyID v) = hashWithSalt s v

instance ToField KeyID where
  toField (KeyID k) = toField k

instance FromField KeyID where
  fromField f mv = KeyID <$> fromField f mv

instance Num KeyID where
  KeyID c1 + KeyID c2 = KeyID (c1 + c2)
  {-# INLINABLE (+) #-}

  KeyID c1 - KeyID c2 = KeyID (c1 - c2)
  {-# INLINABLE (-) #-}

  KeyID c1 * KeyID c2 = KeyID (c1 * c2)
  {-# INLINABLE (*) #-}

  abs (KeyID c) = KeyID (abs c)
  {-# INLINABLE abs #-}

  negate (KeyID c) = KeyID (negate c)
  {-# INLINABLE negate #-}

  signum (KeyID c) = KeyID (signum c)
  {-# INLINABLE signum #-}

  fromInteger = KeyID . fromInteger
  {-# INLINABLE fromInteger #-}

instance FromJSON KeyID where
  parseJSON = withScientific "KeyID" $ \t ->
    pure $ KeyID $ fromMaybe 0 $ toBoundedInteger t

instance ToJSON KeyID where
  toJSON (KeyID k) = toJSON k

data DeviceID = DeviceID {unDeviceID :: Int64}
  deriving (Show, Eq, Ord)

instance Hashable DeviceID where
  hashWithSalt s (DeviceID v) = hashWithSalt s v

instance ToField DeviceID where
  toField (DeviceID k) = toField k

instance FromField DeviceID where
  fromField f mv = DeviceID <$> fromField f mv

instance Num DeviceID where
  DeviceID c1 + DeviceID c2 = DeviceID (c1 + c2)
  {-# INLINABLE (+) #-}

  DeviceID c1 - DeviceID c2 = DeviceID (c1 - c2)
  {-# INLINABLE (-) #-}

  DeviceID c1 * DeviceID c2 = DeviceID (c1 * c2)
  {-# INLINABLE (*) #-}

  abs (DeviceID c) = DeviceID (abs c)
  {-# INLINABLE abs #-}

  negate (DeviceID c) = DeviceID (negate c)
  {-# INLINABLE negate #-}

  signum (DeviceID c) = DeviceID (signum c)
  {-# INLINABLE signum #-}

  fromInteger = DeviceID . fromInteger
  {-# INLINABLE fromInteger #-}

instance FromJSON DeviceID where
  parseJSON = withScientific "DeviceID" $ \t ->
    pure $ DeviceID $ fromMaybe 0 $ toBoundedInteger t

instance ToJSON DeviceID where
  toJSON (DeviceID k) = toJSON k

type Meta      = Value

data Device = Device
  { devID        :: DeviceID
  , devKeyId     :: KeyID
  , devToken     :: Token
  , devUUID      :: UUID
  , devAddr      :: Addr
  , devGwId      :: DeviceID
  , devMeta      :: Value
  , devPingAt    :: CreatedAt
  , devKey       :: Key
  , devCreatedAt :: CreatedAt
  }
  deriving (Show)

instance FromRow Device where
  fromRow = do
    devID <- field
    devKeyId <- field
    devToken <- field
    devUUID <- field
    devAddr <- field
    devGwId <- field
    devMeta <- fromMaybe Null <$> field
    devCreatedAt <- field
    return Device
      { devPingAt = 0
      , devKey = ""
      , ..
      }

instance ToJSON Device where
  toJSON Device {..} = object
    [ "id"         .= devID
    , "key"        .= devKey
    , "token"      .= devToken
    , "uuid"       .= devUUID
    , "meta"       .= devMeta
    , "addr"       .= devAddr
    , "gw_id"      .= devGwId
    , "ping_at"    .= devPingAt
    , "created_at" .= devCreatedAt
    ]

instance FromJSON Device where
  parseJSON = withObject "Device" $ \o -> do
    devID        <- o .: "id"
    devKey       <- o .: "key"
    devToken     <- o .: "token"
    devUUID      <- o .: "uuid"
    devAddr      <- o .: "addr"
    devGwId      <- o .: "gw_id"
    devMeta      <- o .: "meta"
    devPingAt    <- o .: "ping_at"
    devCreatedAt <- o .: "created_at"
    return Device{devKeyId = 0, ..}
