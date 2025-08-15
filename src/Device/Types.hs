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

  , MetricID (..)
  , Metric (..)

  , IndexNameId (..)
  , IndexName (..)

  , EmqxUser (..)
  , EmqxMountPoint (..)
  ) where

import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      object, withObject, withScientific,
                                      withText, (.:), (.:?), (.=))
import           Data.Hashable       (Hashable (..))
import           Data.Int            (Int64)
import           Data.Maybe          (fromMaybe)
import           Data.Scientific     (toBoundedInteger)
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import           Database.PSQL.Types (FromField (..), FromRow (..),
                                      ToField (..), field)
import           Text.Read           (readMaybe)


newtype Key = Key {unKey :: Text}
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


newtype Addr = Addr {unAddr :: Text}
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

newtype Token = Token {unToken :: Text}
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

newtype UUID = UUID {unUUID :: Text}
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


newtype CreatedAt = CreatedAt {unCreatedAt :: Int64}
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


newtype KeyID = KeyID {unKeyID :: Int64}
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

newtype DeviceID = DeviceID {unDeviceID :: Int64}
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

newtype MetricID = MetricID {unMetricID :: Int64}
  deriving (Show, Eq, Ord)

instance Hashable MetricID where
  hashWithSalt s (MetricID v) = hashWithSalt s v

instance ToField MetricID where
  toField (MetricID k) = toField k

instance FromField MetricID where
  fromField f mv = MetricID <$> fromField f mv

instance Num MetricID where
  MetricID c1 + MetricID c2 = MetricID (c1 + c2)
  {-# INLINABLE (+) #-}

  MetricID c1 - MetricID c2 = MetricID (c1 - c2)
  {-# INLINABLE (-) #-}

  MetricID c1 * MetricID c2 = MetricID (c1 * c2)
  {-# INLINABLE (*) #-}

  abs (MetricID c) = MetricID (abs c)
  {-# INLINABLE abs #-}

  negate (MetricID c) = MetricID (negate c)
  {-# INLINABLE negate #-}

  signum (MetricID c) = MetricID (signum c)
  {-# INLINABLE signum #-}

  fromInteger = MetricID . fromInteger
  {-# INLINABLE fromInteger #-}

instance FromJSON MetricID where
  parseJSON = withScientific "MetricID" $ \t ->
    pure $ MetricID $ fromMaybe 0 $ toBoundedInteger t

instance ToJSON MetricID where
  toJSON (MetricID k) = toJSON k

type Meta      = Value

data Device = Device
  { devID        :: DeviceID
  , devKeyId     :: KeyID
  , devToken     :: Token
  , devUUID      :: UUID
  , devAddr      :: Addr
  , devGwId      :: DeviceID
  , devMeta      :: Value
  , devMetric    :: Value
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
      , devMetric = Null
      , ..
      }

instance ToJSON Device where
  toJSON Device {..} = object
    [ "id"         .= devID
    , "key_id"     .= devKeyId
    , "key"        .= devKey
    , "token"      .= devToken
    , "uuid"       .= devUUID
    , "meta"       .= devMeta
    , "metric"     .= devMetric
    , "addr"       .= devAddr
    , "gw_id"      .= devGwId
    , "ping_at"    .= devPingAt
    , "created_at" .= devCreatedAt
    ]

instance FromJSON Device where
  parseJSON = withObject "Device" $ \o -> do
    devID        <- o .: "id"
    devKeyId     <- o .: "key_id"
    devKey       <- o .: "key"
    devToken     <- o .: "token"
    devUUID      <- o .: "uuid"
    devAddr      <- o .: "addr"
    devGwId      <- o .: "gw_id"
    devMeta      <- o .: "meta"
    devMetric    <- o .: "metric"
    devPingAt    <- o .: "ping_at"
    devCreatedAt <- o .: "created_at"
    return Device{..}


data Metric = Metric
  { metricId        :: MetricID
  , metricDevId     :: DeviceID
  , metricField     :: String
  , metricRawValue  :: String
  , metricValue     :: Float
  , metricCreatedAt :: CreatedAt
  }
  deriving (Show)

instance FromRow Metric where
  fromRow = do
    metricId <- field
    metricDevId <- field
    metricField <- field
    metricRawValue <- field
    metricValue <- field
    metricCreatedAt <- field
    return Metric { .. }

instance ToJSON Metric where
  toJSON Metric {..} = object
    [ "id"         .= metricId
    , "dev_id"     .= metricDevId
    , "field"      .= metricField
    , "raw_value"  .= metricRawValue
    , "value"      .= metricValue
    , "created_at" .= metricCreatedAt
    ]


parseValue :: Maybe String -> Maybe Float -> (String, Float)
parseValue Nothing Nothing     = ("", 0)
parseValue (Just v0) Nothing   = (v0, fromMaybe 0 $ readMaybe v0)
parseValue Nothing (Just v1)   = (show v1, v1)
parseValue (Just v0) (Just v1) = (v0, v1)


instance FromJSON Metric where
  parseJSON = withObject "Metric" $ \o -> do
    metricId        <- o .: "id"
    metricDevId     <- o .: "dev_id"
    metricField     <- o .: "field"
    rv              <- o .:? "raw_value"
    v               <- o .:? "value"
    metricCreatedAt <- o .: "created_at"

    let (metricRawValue, metricValue) = parseValue rv v
    return Metric{..}


newtype IndexName = IndexName {unIndexName :: Text}
  deriving (Show, Eq, Ord)

instance Hashable IndexName where
  hashWithSalt s (IndexName v) = hashWithSalt s v


instance ToField IndexName where
  toField (IndexName k) = toField k

instance FromField IndexName where
  fromField f mv = IndexName <$> fromField f mv

instance IsString IndexName where
  fromString = IndexName . fromString

instance FromJSON IndexName where
  parseJSON = withText "IndexName" $ \t -> pure $ IndexName t

instance ToJSON IndexName where
  toJSON (IndexName k) = toJSON k


newtype IndexNameId = IndexNameId {unIndexNameId :: Int64}
  deriving (Show, Eq, Ord)

instance Hashable IndexNameId where
  hashWithSalt s (IndexNameId v) = hashWithSalt s v

instance ToField IndexNameId where
  toField (IndexNameId k) = toField k

instance FromField IndexNameId where
  fromField f mv = IndexNameId <$> fromField f mv

instance Num IndexNameId where
  IndexNameId c1 + IndexNameId c2 = IndexNameId (c1 + c2)
  {-# INLINABLE (+) #-}

  IndexNameId c1 - IndexNameId c2 = IndexNameId (c1 - c2)
  {-# INLINABLE (-) #-}

  IndexNameId c1 * IndexNameId c2 = IndexNameId (c1 * c2)
  {-# INLINABLE (*) #-}

  abs (IndexNameId c) = IndexNameId (abs c)
  {-# INLINABLE abs #-}

  negate (IndexNameId c) = IndexNameId (negate c)
  {-# INLINABLE negate #-}

  signum (IndexNameId c) = IndexNameId (signum c)
  {-# INLINABLE signum #-}

  fromInteger = IndexNameId . fromInteger
  {-# INLINABLE fromInteger #-}

instance FromJSON IndexNameId where
  parseJSON = withScientific "IndexNameId" $ \t ->
    pure $ IndexNameId $ fromMaybe 0 $ toBoundedInteger t

instance ToJSON IndexNameId where
  toJSON (IndexNameId k) = toJSON k

newtype EmqxMountPoint = EmqxMountPoint String deriving (Show)

instance ToJSON EmqxMountPoint where
  toJSON (EmqxMountPoint uuid) = object [ "mountpoint" .= uuid ]

data EmqxUser = EmqxSuperAdmin
    | EmqxAdmin EmqxMountPoint
    | EmqxNormal EmqxMountPoint
    deriving (Show)

instance ToJSON EmqxUser where
  toJSON EmqxSuperAdmin               = object ["type" .= ("superadmin" :: String)]
  toJSON (EmqxAdmin (EmqxMountPoint mp))  = object ["type" .= ("admin" :: String), "mountpoint" .= mp]
  toJSON (EmqxNormal (EmqxMountPoint mp)) = object ["type" .= ("normal" :: String), "mountpoint" .= mp]
