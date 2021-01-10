{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Fetcher.Updates where

import           Bot.Telegram.SharedTypes (Contact, Dice, FileInfo, Location,
                                           Venue)
import           Data.Aeson
import           Data.Text                (Text)


-- * Data types section


newtype Updates = Updates { extractUpdates :: [Upd] }


data Upd = Upd
  { updateId :: Integer
  , message  :: ReceivedMsg
  }


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
  }


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
