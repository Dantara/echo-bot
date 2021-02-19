{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Types.Updates where

import           Bot.Telegram.Types.Shared (Contact, Dice, FileInfo, Location,
                                            Venue)
import           Data.Aeson                (FromJSON (parseJSON), withObject,
                                            (.:), (.:?))
import           Data.Text                 (Text)


-- * Data types section


newtype Updates = Updates { extractUpdates :: [Upd] }
  deriving (Show, Eq)


data Upd = Upd
  { updateId :: Integer
  , message  :: ReceivedMsg
  } deriving (Show, Eq)


data ReceivedMsg = ReceivedMsg
  { receivedChatId          :: Integer
  , receivedMessageId       :: Integer
  , receivedText            :: Maybe Text
  , receivedAudio           :: Maybe FileInfo
  , receivedDocument        :: Maybe FileInfo
  , receivedPhoto           :: Maybe [FileInfo]
  , receivedSticker         :: Maybe FileInfo
  , receivedVideo           :: Maybe FileInfo
  , receivedVideoNote       :: Maybe FileInfo
  , receivedVoice           :: Maybe FileInfo
  , receivedCaption         :: Maybe Text
  , receivedContact         :: Maybe Contact
  , receivedDice            :: Maybe Dice
  , receivedVenue           :: Maybe Venue
  , receivedMessageLocation :: Maybe Location
  } deriving (Show, Eq)


-- * Parsing section


instance FromJSON Updates where
  parseJSON = withObject "Updates" $ \us -> Updates
    <$> us .: "result"


instance FromJSON Upd where
  parseJSON = withObject "Update" $ \upd -> Upd
    <$> upd .: "update_id"
    <*> upd .: "message"


instance FromJSON ReceivedMsg where
  parseJSON = withObject "ReceivedMsg" $ \msg -> ReceivedMsg
    <$> (msg .: "chat" >>= (.: "id"))
    <*> msg .: "message_id"
    <*> msg .:? "text"
    <*> msg .:? "audio"
    <*> msg .:? "document"
    <*> msg .:? "photo"
    <*> msg .:? "sticker"
    <*> msg .:? "video"
    <*> msg .:? "video_note"
    <*> msg .:? "voice"
    <*> msg .:? "caption"
    <*> msg .:? "contact"
    <*> msg .:? "dice"
    <*> msg .:? "venue"
    <*> msg .:? "location"
