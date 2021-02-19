{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types.Shared where

import           Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import           Data.Text  (Text)


data Attachment
  = Photo FileInfo
  | Video FileInfo
  | Audio FileInfo
  | AudioMsg FileInfo
  | Document FileInfo
  | Wall FileInfo
  | Market FileInfo
  | Poll FileInfo
  | Sticker Integer
  | UnknownAttachment
  deriving (Eq, Show)


data FileInfo = FileInfo
  { mediaId   :: Integer
  , ownerId   :: Integer
  , accessKey :: Maybe Text
  } deriving (Eq, Show)


instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \a -> do
    type' <- a .: "type"
    case type' :: Text of
      "photo" ->
        Photo <$> (a .: "photo")
      "video" ->
        Video <$> (a .: "video")
      "audio" ->
        Audio <$> (a .: "audio")
      "audio_message" ->
        AudioMsg <$> (a .: "audio_message")
      "doc" ->
        Document <$> (a .: "doc")
      "wall" ->
        Wall <$> (a .: "wall")
      "market" ->
        Market <$> (a .: "market")
      "poll" ->
        Poll <$> (a .: "poll")
      "sticker" ->
        Sticker <$> (a .: "sticker" >>= (.: "sticker_id"))
      _ ->
        pure UnknownAttachment


instance FromJSON FileInfo where
  parseJSON = withObject "FileInfo" $ \f -> FileInfo
    <$> f .: "id"
    <*> f .: "owner_id"
    <*> f .:? "access_key"
