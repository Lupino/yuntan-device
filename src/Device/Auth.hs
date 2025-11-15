{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Device.Auth
  ( genTokenHandler
  , requireAdmin
  , requirePerm
  , requireManager
  , requireIndexName
  ) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (lift)
import           Crypto.Random          (MonadRandom)
import           Data.Aeson             (FromJSON (..), ToJSON (..), object,
                                         withObject, withText, (.!=), (.:?),
                                         (.=))
import qualified Data.Aeson             as Aeson (decodeStrict, encode)
import           Data.Aeson.Types       (Parser)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as L (toStrict)
import           Data.Int               (Int64)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as LT (Text, drop, toStrict)
import           Database.PSQL          (HasPSQL)
import           Device.API             (countIndex, getIndexNameId_)
import           Device.Types           (Device (..), DeviceID, IndexName)
import           Device.Util            (getEpochTime, parseIndexName)
import           Jose.Jwa               (JwsAlg (HS256))
import           Jose.Jwk               (Jwk (SymmetricJwk))
import           Jose.Jwt               (Jwt (..), JwtContent (..),
                                         JwtEncoding (..), JwtError,
                                         Payload (..))
import qualified Jose.Jwt               as Jwt (decode, encode)
import           Network.HTTP.Types     (status401, status403)
import           Web.Scotty.Haxl        (ActionH)
import           Web.Scotty.Trans       (header, jsonData)
import           Web.Scotty.Utils       (err, errBadRequest, ok, safeQueryParam)

data Role = RoleAdmin | RoleNormal | Role Text | RoleEmpty
  deriving (Show, Eq, Ord)

instance FromJSON Role where
  parseJSON = withText "Role" parse
    where parse :: Text -> Parser Role
          parse "admin"  = pure RoleAdmin
          parse "normal" = pure RoleNormal
          parse "empty"  = pure RoleEmpty
          parse t        = pure $ Role t

instance ToJSON Role where
  toJSON RoleAdmin  = toJSON ("admin" :: Text)
  toJSON RoleNormal = toJSON ("normal" :: Text)
  toJSON RoleEmpty  = toJSON ("empty" :: Text)
  toJSON (Role t)   = toJSON t

data AuthInfo = AuthInfo
  { authRole      :: Role
  , authIndexList :: [IndexName]
  , authManager   :: Maybe IndexName
  , authUser      :: Maybe IndexName
  , authDevId     :: Maybe DeviceID
  , authIssueAt   :: Maybe Int64
  , authExpireAt  :: Maybe Int64
  , authNonce     :: String
  }
  deriving (Show, Eq, Ord)

instance FromJSON AuthInfo where
  parseJSON = withObject "AuthInfo" $ \o -> do
    authRole      <- o .:? "rol" .!= RoleEmpty
    authIndexList <- o .:? "ins" .!= []
    authDevId     <- o .:? "did"
    authManager   <- o .:? "min"
    authIssueAt   <- o .:? "iat"
    authUser      <- o .:? "sub"
    authExpireAt  <- o .:? "exp"
    authNonce     <- o .:? "jti" .!= ""
    return AuthInfo{..}

instance ToJSON AuthInfo where
  toJSON AuthInfo {..} = object
    [ "rol" .= authRole
    , "did" .= authDevId
    , "min" .= authManager
    , "ins" .= authIndexList
    , "iat" .= authIssueAt
    , "exp" .= authExpireAt
    , "sub" .= authUser
    , "jti" .= authNonce
    ]


allAuthIndexList :: AuthInfo -> [IndexName]
allAuthIndexList AuthInfo {..} =
  authIndexList ++ catMaybes [authManager, authUser]

encodeJwt :: MonadRandom m => ByteString -> AuthInfo -> m (Either JwtError Jwt)
encodeJwt key info = Jwt.encode [jwk]  (JwsEncoding HS256) (Claims . L.toStrict $ Aeson.encode info)
  where jwk = SymmetricJwk key Nothing Nothing Nothing


decodeJwt :: MonadRandom m => ByteString -> LT.Text -> m (Maybe AuthInfo)
decodeJwt key bearerToken = do
  decoded <- Jwt.decode [jwk] (Just (JwsEncoding HS256)) token
  case decoded of
    Left _              -> pure Nothing
    Right (Jws (_, bs)) -> pure $ Aeson.decodeStrict bs
    Right (Jwe (_, bs)) -> pure $ Aeson.decodeStrict bs
    Right (Unsecured _) -> pure Nothing

  where jwk = SymmetricJwk key Nothing Nothing Nothing
        token = encodeUtf8 . LT.toStrict $ LT.drop 7 bearerToken

getAuthInfo :: ByteString -> ActionH u w (Maybe AuthInfo)
getAuthInfo key = do
  mBearerToken <- header "authorization"
  case mBearerToken of
    Nothing          -> pure Nothing
    Just bearerToken -> do
      decoded <- liftIO $ decodeJwt key bearerToken
      case decoded of
        Just authInfo -> pure $ Just authInfo
        _             -> pure Nothing


checkExpire :: AuthInfo -> ActionH u w () -> ActionH u w ()
checkExpire AuthInfo {authExpireAt = Nothing} next = next
checkExpire AuthInfo {authExpireAt = Just expAt} next = do
  now <- getEpochTime
  if expAt > now then next
                 else err status401 "expired"

requireAuth :: ByteString -> (AuthInfo -> ActionH u w ()) -> ActionH u w ()
requireAuth key next = do
  mAuthInfo <- getAuthInfo key
  case mAuthInfo of
    Nothing       -> err status401 "Unauthorized"
    Just authInfo -> next authInfo

noPermessions :: ActionH u w ()
noPermessions = err status403 "No permessions"

requireAdmin :: Bool -> ByteString -> ActionH u w () -> ActionH u w ()
requireAdmin False _ next = next
requireAdmin True key next = do
  requireAuth key $ \authInfo ->
    checkExpire authInfo
    $ checkAdmin (authRole authInfo) next
    noPermessions


requireManager :: HasPSQL u => Bool -> ByteString -> (Device -> ActionH u w ()) -> Device -> ActionH u w ()
requireManager False _ next dev = next dev
requireManager True key next dev = do
  requireAuth key $ \authInfo ->
    checkExpire authInfo
    $ checkAdmin (authRole authInfo) (next dev)
    $ checkIndex (catMaybes [authManager authInfo, authUser authInfo]) next dev
    $ checkDevId (authDevId authInfo) next dev
    noPermessions


requirePerm :: HasPSQL u => Bool -> ByteString -> (Device -> ActionH u w ()) -> Device -> ActionH u w ()
requirePerm False _ next dev = next dev
requirePerm True key next dev = do
  requireAuth key $ \authInfo ->
    checkExpire authInfo
    $ checkAdmin (authRole authInfo) (next dev)
    $ checkIndex (allAuthIndexList authInfo) next dev
    $ checkDevId (authDevId authInfo) next dev
    noPermessions

checkAdmin :: Role -> ActionH u w () -> ActionH u w () -> ActionH u w ()
checkAdmin RoleAdmin next _ = next
checkAdmin _ _ nextCheck    = nextCheck

checkDevId :: Maybe DeviceID -> (Device -> ActionH u w ()) -> Device -> ActionH u w () -> ActionH u w ()
checkDevId Nothing _ _ nextCheck = nextCheck
checkDevId (Just did) next dev nextCheck
  | did == devID dev = next dev
  | otherwise = nextCheck

checkIndex
  :: HasPSQL u
  => [IndexName] -> (Device -> ActionH u w ()) -> Device -> ActionH u w () -> ActionH u w ()
checkIndex [] _ _ nextCheck = nextCheck
checkIndex names next dev nextCheck = do
  nids <- lift $ catMaybes <$> mapM getIndexNameId_ names
  if null nids then
    nextCheck
  else do
    count <- lift $ countIndex nids (Just $ devID dev)
    if count > 0 then
      next dev
    else
      nextCheck


requireIndexName :: Monoid w => Bool -> ByteString -> ([IndexName] -> ActionH u w ()) -> ActionH u w ()
requireIndexName False _ next = do
  names <- parseIndexName <$> safeQueryParam "index_name" ""
  next names
requireIndexName True key next = do
  names <- parseIndexName <$> safeQueryParam "index_name" ""
  requireAuth key $ \authInfo -> do
    checkExpire authInfo
    $ checkAdmin (authRole authInfo) (next names)
    $ doCheckIndexName (allAuthIndexList authInfo) names next
    noPermessions

doCheckIndexName :: [IndexName] -> [IndexName] -> ([IndexName] -> ActionH u w ()) -> ActionH u w () -> ActionH u w ()
doCheckIndexName [] _ _ nextCheck = nextCheck
doCheckIndexName expect [] next _ = next expect
doCheckIndexName expect names next nextCheck
  | allIn names = next names
  | otherwise = nextCheck
  where allIn :: [IndexName] -> Bool
        allIn [] = True
        allIn (x:xs)
          | x `elem` expect = allIn xs
          | otherwise = False

genTokenHandler :: ByteString -> ActionH u w ()
genTokenHandler key = do
  authInfo <- jsonData
  eJwt <- liftIO $ encodeJwt key authInfo
  case eJwt of
    Left e    -> errBadRequest $ show e
    Right jwt -> ok "token" $ decodeUtf8 $ unJwt jwt
