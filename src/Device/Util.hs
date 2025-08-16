{-# LANGUAGE OverloadedStrings #-}

module Device.Util
  ( getEpochTime
  , parseIndexName
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Int               (Int64)
import           Data.Text              (Text)
import qualified Data.Text              as T (filter, splitOn)
import           Data.UnixTime
import           Device.Types           (IndexName (..))
import           Foreign.C.Types        (CTime (..))

getEpochTime :: MonadIO m => m Int64
getEpochTime = liftIO $ un . toEpochTime <$> getUnixTime
  where un :: CTime -> Int64
        un (CTime t) = t

removeAllSpaces :: Text -> Text
removeAllSpaces = T.filter (/= ' ')

parseIndexName :: Text -> [IndexName]
parseIndexName = go . T.splitOn "," . removeAllSpaces
  where go :: [Text] -> [IndexName]
        go []      = []
        go ("":xs) = go xs
        go (x:xs)  = IndexName x : go xs
