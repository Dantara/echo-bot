{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types.Shared where

import           Data.Aeson
import           Data.Text  (Text)


data FileInfo = FileInfo
  { fileId  :: Integer
  , ownerId :: Integer
  } deriving (Eq)


instance FromJSON FileInfo where
  parseJSON = withObject "FileInfo" $ \f -> FileInfo
    <$> f .: "id"
    <*> f .: "owner_id"
