{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.Telegram where

import           Bot
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Aeson
import           Data.Char                     (toLower)
import           Data.Text                     (Text)
import           GHC.Generics
import           Helpers
import           Logger
import           Network.HTTP.Req


newtype TelegramBot a = TelegramBot { unwrapBot :: ReaderT (BotEnv Msg) IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader (BotEnv Msg)
                   )


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
  , receivedPoll            :: Maybe ()
  }

data Msg = Msg
  { chatId     :: Integer
  , msgContent :: MsgContent
  }

data MsgContent
  = TextContent Text
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
  | PollContent Integer
  | UnsupportedContent Integer


instance ToJSON Msg where
  toJSON (Msg ci (TextContent t))
    = object ["chat_id" .= ci, "text" .= t]
  toJSON (Msg ci (AudioContent (FileInfo t)))
    = object ["chat_id" .= ci, "audio" .= t]
  toJSON (Msg ci (DocumentContent (FileInfo t) (Just c)))
    = object ["chat_id" .= ci, "document" .= t, "caption" .= c]
  toJSON (Msg ci (DocumentContent (FileInfo t) Nothing))
    = object ["chat_id" .= ci, "document" .= t]
  toJSON (Msg ci (PhotoContent (FileInfo t) (Just c)))
    = object ["chat_id" .= ci, "photo" .= t, "caption" .= c]
  toJSON (Msg ci (PhotoContent (FileInfo t) Nothing))
    = object ["chat_id" .= ci, "photo" .= t]
  toJSON (Msg ci (StickerContent (FileInfo t)))
    = object ["chat_id" .= ci, "sticker" .= t]
  toJSON (Msg ci (VideoContent (FileInfo t) (Just c)))
    = object ["chat_id" .= ci, "video" .= t, "caption" .= c]
  toJSON (Msg ci (VideoContent (FileInfo t) Nothing))
    = object ["chat_id" .= ci, "video" .= t]
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
  toJSON (Msg ci (PollContent i))
    = object [ "chat_id" .= ci
             , "from_chat_id" .= ci
             , "message_id" .= i]
  toJSON (Msg ci (UnsupportedContent i))
    = object [ "chat_id" .= ci
             , "from_chat_id" .= ci
             , "message_id" .= i]


receivedMsgToMsg :: ReceivedMsg -> Msg
receivedMsgToMsg (ReceivedMsg ci _ (Just t) _ _ _ _ _ _ _ _ _ _ _ _ _)
  = Msg ci (TextContent t)
receivedMsgToMsg (ReceivedMsg ci _ _ (Just f) _ _ _ _ _ _ _ _ _ _ _ _)
  = Msg ci (AudioContent f)
receivedMsgToMsg (ReceivedMsg ci _ _ _ (Just f) _ _ _ _ _ c _ _ _ _ _)
  = Msg ci (DocumentContent f c)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ (Just fs) _ _ _ _ c _ _ _ _ _)
  = Msg ci (PhotoContent (last fs) c)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ (Just f) _ _ _ _ _ _ _ _ _)
  = Msg ci (StickerContent f)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ (Just f) _ _ c _ _ _ _ _)
  = Msg ci (VideoContent f c)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ (Just f) _ _ _ _ _ _ _)
  = Msg ci (VideoNoteContent f)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ (Just f) _ _ _ _ _ _)
  = Msg ci (VoiceContent f)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ (Just c) _ _ _ _)
  = Msg ci (ContactContent c)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ _ (Just d) _ _ _)
  = Msg ci (DiceContent d)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ _ _ (Just v) _ _)
  = Msg ci (VenueContent v)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ _ _ _ (Just l) _)
  = Msg ci (LocationContent l)
receivedMsgToMsg (ReceivedMsg ci i _ _ _ _ _ _ _ _ _ _ _ _ _ (Just ()))
  = Msg ci (PollContent i)
receivedMsgToMsg (ReceivedMsg ci i _ _ _ _ _ _ _ _ _ _ _ _ _ _)
  = Msg ci (UnsupportedContent i)


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
    <*> msg .:? "poll"

data Contact = Contact
  { phoneNumber :: Text
  , firstName   :: Text
  , lastName    :: Maybe Text
  , vcard       :: Maybe Text
  } deriving (Generic)

instance FromJSON Contact where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelToSnakeCase }

newtype Dice = Dice { diceEmoji :: Text }
  deriving (Generic)

instance FromJSON Dice where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 4 }

data Location = Location
  { longitude :: Float
  , latitude  :: Float
  } deriving (Generic, FromJSON)

data Venue = Venue
  { venueLocation :: Location
  , venueTitle    :: Text
  , venueAddress  :: Text
  } deriving (Generic)

instance FromJSON Venue where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 5 }

newtype FileInfo = FileInfo { fileId :: Text }

instance FromJSON FileInfo where
  parseJSON = withObject "FileInfo" $ \f -> FileInfo
    <$> f .: "file_id"


instance Logger TelegramBot where
  log = logSTD
  getLogLevel = asks logLevel

instance BotTypes TelegramBot where
  data Update TelegramBot = Update
    { updateId :: Integer
    , message  :: ReceivedMsg
    }

  type Message TelegramBot = Msg

instance ProducerBot TelegramBot where
  pullUpdates = do
    o <- getOffset

    let payload = object
          [ "offset" .= (o + 1)
          ]

    token' <- ("bot" <>) . extractToken <$> getToken

    logDebug "Fetching updates from Telegram"

    r <- runReq defaultHttpConfig
      $ req
          POST
          (https "api.telegram.org" /: token' /: "getUpdates")
          (ReqBodyJson payload)
          jsonResponse
          mempty

    pure $ extractUpdates $ responseBody r

  updateToMessage = receivedMsgToMsg . message

  offsetOfUpdate = updateId


instance ConsumerBot TelegramBot where
  sendMessage m = do
    token' <- ("bot" <>) . extractToken <$> getToken

    logDebug "Sending Telegram response"

    _ <- runReq defaultHttpConfig
      $ req
          POST
          (https "api.telegram.org" /: token' /: "sendMessage")
          (ReqBodyJson m)
          ignoreResponse
          mempty

    pure ()

instance HasOffset TelegramBot where
  getOffset = asks offset >>= liftIO . readTVarIO

  updateOffset o = asks offset >>= \t ->
    liftIO $ atomically $ writeTVar t o


instance HasMessageQueue TelegramBot where
  pullMessage = asks tMessages
    >>= liftIO . atomically . tryReadTQueue

  pushMessage m = asks tMessages >>= \q ->
    liftIO $ atomically $ writeTQueue q m


instance HasToken TelegramBot where
  getToken = asks token


instance FromJSON (Update TelegramBot) where
  parseJSON = withObject "Update" $ \upd -> Update
    <$> upd .: "update_id"
    <*> upd .: "message"


newtype Updates = Updates { extractUpdates :: [Update TelegramBot] }


instance FromJSON Updates where
  parseJSON = withObject "Updates" $ \us -> Updates
    <$> us .: "result"


runBot :: TelegramBot a -> BotEnv Msg -> IO a
runBot app = runReaderT (unwrapBot app)


loopBot :: TelegramBot a -> BotEnv Msg -> (BotEnv Msg -> Int) -> IO ()
loopBot app env f = void $ forkIO $ forever $ do
  _ <- runBot app env
  threadDelay $ f env
