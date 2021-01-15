{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types.Shared where

import           Data.Aeson
import           Data.Text  (Text)


data Attachment
  = Photo FileInfo
  | Document FileInfo
  | UnknownAttachment
  deriving (Eq)


data FileInfo = FileInfo
  { fileId  :: Integer
  , ownerId :: Integer
  } deriving (Eq)


instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \a -> do
    type' <- a .: "type"
    case type' :: Text of
      "photo" ->
        Photo <$> (a .: "photo")
      "doc" ->
        Document <$> (a .: "doc")
      _ ->
        pure UnknownAttachment


instance FromJSON FileInfo where
  parseJSON = withObject "FileInfo" $ \f -> FileInfo
    <$> f .: "id"
    <*> f .: "owner_id"
