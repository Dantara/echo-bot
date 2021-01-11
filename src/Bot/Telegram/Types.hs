{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Types where

import           Bot          (Command (..))
import           Data.Aeson
import           Data.Char    (toLower)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Helpers      (camelToSnakeCase)


-- * Data types section


data Msg = Msg
  { chatId     :: Integer
  , msgContent :: MsgContent
  }


data MsgContent
  = CommandContent Command
  | TextContent Text
  | AudioContent FileInfo
  | DocumentContent FileInfo (Maybe Text)
  | PhotoContent FileInfo (Maybe Text)
  | StickerContent FileInfo
  | VideoContent FileInfo (Maybe Text)
  | VideoNoteContent FileInfo
  | VoiceContent FileInfo
  | ContactContent Contact
  | DiceContent Dice
  | VenueContent Venue
  | LocationContent Location
  | UnsupportedContent Integer


newtype FileInfo = FileInfo { fileId :: Text }


data Contact = Contact
  { phoneNumber :: Text
  , firstName   :: Text
  , lastName    :: Maybe Text
  , vcard       :: Maybe Text
  } deriving (Generic)


newtype Dice = Dice { diceEmoji :: Text }
  deriving (Generic)


data Location = Location
  { longitude :: Float
  , latitude  :: Float
  } deriving (Generic, FromJSON)


data Venue = Venue
  { venueLocation :: Location
  , venueTitle    :: Text
  , venueAddress  :: Text
  } deriving (Generic)


-- * Serialization section


-- | ToJSON Serialization instance for Msg data type
instance ToJSON Msg where
  toJSON (Msg ci (CommandContent (HelpCommand t)))
    = object ["chat_id" .= ci, "text" .= t]

  toJSON (Msg ci (CommandContent (RepeatCommand t)))
    = object [ "chat_id" .= ci
             , "text" .= t
             , "reply_markup" .= object
               [ "keyboard" .= toJSON
                 [ [ object [ "text" .= ("1" :: Text)]
                   , object [ "text" .= ("2" :: Text)]
                   ]
                 , [ object [ "text" .= ("3" :: Text)]
                   , object [ "text" .= ("4" :: Text)]
                   ]
                 , [object [ "text" .= ("5" :: Text)] ]
                 ]
               ]
             ]

  toJSON (Msg ci (TextContent t))
    = object [ "chat_id" .= ci
             , "text" .= t
             , "reply_markup" .= object
               [ "remove_keyboard" .= True
               ]
             ]

  toJSON (Msg ci (AudioContent (FileInfo t)))
    = object ["chat_id" .= ci, "audio" .= t]

  toJSON (Msg ci (DocumentContent (FileInfo t) c))
    = object [ "chat_id" .= ci
             , "document" .= t
             , "caption" .= c
             ]

  toJSON (Msg ci (PhotoContent (FileInfo t) c))
    = object [ "chat_id" .= ci
             , "photo" .= t
             , "caption" .= c
             ]

  toJSON (Msg ci (StickerContent (FileInfo t)))
    = object ["chat_id" .= ci, "sticker" .= t]

  toJSON (Msg ci (VideoContent (FileInfo t) c))
    = object [ "chat_id" .= ci
             , "video" .= t
             , "caption" .= c
             ]

  toJSON (Msg ci (VideoNoteContent (FileInfo t)))
    = object ["chat_id" .= ci, "video_note" .= t]

  toJSON (Msg ci (VoiceContent (FileInfo t)))
    = object ["chat_id" .= ci, "voice" .= t]

  toJSON (Msg ci (ContactContent (Contact pn fn ln vc)))
    = object [ "chat_id" .= ci
             , "phone_number" .= pn
             , "fist_name" .= fn
             , "last_name" .= ln
             , "vcard" .= vc
             ]

  toJSON (Msg ci (DiceContent (Dice de)))
    = object ["chat_id" .= ci, "emoji" .= de]

  toJSON (Msg ci (VenueContent (Venue (Location lo la) t a)))
    = object [ "chat_id" .= ci
             , "latitude" .= la
             , "longitude" .= lo
             , "title" .= t
             , "address" .= a
             ]

  toJSON (Msg ci (LocationContent (Location lo la)))
    = object [ "chat_id" .= ci
             , "latitide" .= la
             , "longitide" .= lo
             ]

  toJSON (Msg ci (UnsupportedContent i))
    = object [ "chat_id" .= ci
             , "from_chat_id" .= ci
             , "message_id" .= i]


-- * Parsing section


instance FromJSON Contact where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelToSnakeCase }


instance FromJSON Dice where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 4 }


instance FromJSON Venue where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 5 }


instance FromJSON FileInfo where
  parseJSON = withObject "FileInfo" $ \f -> FileInfo
    <$> f .: "file_id"
