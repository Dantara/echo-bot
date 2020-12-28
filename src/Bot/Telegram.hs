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
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           GHC.Generics
import           Helpers
import           Logger
import           Network.HTTP.Req
import           Text.Read


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
  , receivedPoll            :: Maybe () -- Need to fix
  }

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
  | PollContent Integer
  | UnsupportedContent Integer

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
  toJSON (Msg ci (PollContent i))
    = object [ "chat_id" .= ci
             , "from_chat_id" .= ci
             , "message_id" .= i]
  toJSON (Msg ci (UnsupportedContent i))
    = object [ "chat_id" .= ci
             , "from_chat_id" .= ci
             , "message_id" .= i]


receivedMsgToMsg :: ReceivedMsg -> TelegramBot Msg
receivedMsgToMsg (ReceivedMsg ci _ (Just "/help") _ _ _ _ _ _ _ _ _ _ _ _ _)
  = getHelpMsg >>= \h -> pure $ Msg ci (CommandContent (HelpCommand h))
receivedMsgToMsg (ReceivedMsg ci _ (Just "/repeat") _ _ _ _ _ _ _ _ _ _ _ _ _) = do
  q <- getRepsQuestion
  rs <- getRepetitions ci
  let firstLine = "Currently repetitions amount is " <>
                  Text.pack (show rs) <> "\n"
  pure $ Msg ci (CommandContent (RepeatCommand (firstLine <> q)))

receivedMsgToMsg (ReceivedMsg ci _ (Just t) _ _ _ _ _ _ _ _ _ _ _ _ _)
  = pure $ Msg ci (TextContent t)
receivedMsgToMsg (ReceivedMsg ci _ _ (Just f) _ _ _ _ _ _ _ _ _ _ _ _)
  = pure $ Msg ci (AudioContent f)
receivedMsgToMsg (ReceivedMsg ci _ _ _ (Just f) _ _ _ _ _ c _ _ _ _ _)
  = pure $ Msg ci (DocumentContent f c)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ (Just fs) _ _ _ _ c _ _ _ _ _)
  = pure $ Msg ci (PhotoContent (last fs) c)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ (Just f) _ _ _ _ _ _ _ _ _)
  = pure $ Msg ci (StickerContent f)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ (Just f) _ _ c _ _ _ _ _)
  = pure $ Msg ci (VideoContent f c)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ (Just f) _ _ _ _ _ _ _)
  = pure $ Msg ci (VideoNoteContent f)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ (Just f) _ _ _ _ _ _)
  = pure $ Msg ci (VoiceContent f)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ (Just c) _ _ _ _)
  = pure $ Msg ci (ContactContent c)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ _ (Just d) _ _ _)
  = pure $ Msg ci (DiceContent d)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ _ _ (Just v) _ _)
  = pure $ Msg ci (VenueContent v)
receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ _ _ _ (Just l) _)
  = pure $ Msg ci (LocationContent l)
receivedMsgToMsg (ReceivedMsg ci i _ _ _ _ _ _ _ _ _ _ _ _ _ (Just ()))
  = pure $ Msg ci (PollContent i)
receivedMsgToMsg (ReceivedMsg ci i _ _ _ _ _ _ _ _ _ _ _ _ _ _)
  = pure $ Msg ci (UnsupportedContent i)


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
  sendMessage m = case msgContent m of
    CommandContent _ ->
      genericSendMessage m "sendMessage"
    TextContent _ ->
      genericSendMessage m "sendMessage"
    AudioContent _ ->
      genericSendMessage m "sendAudio"
    DocumentContent _ _ ->
      genericSendMessage m "sendDocument"
    PhotoContent _ _ ->
      genericSendMessage m "sendPhoto"
    StickerContent _ ->
      genericSendMessage m "sendSticker"
    VideoContent _ _ ->
      genericSendMessage m "sendVideo"
    VideoNoteContent _ ->
      genericSendMessage m "sendVideoNote"
    VoiceContent _ ->
      genericSendMessage m "sendVoice"
    ContactContent _ ->
      genericSendMessage m "sendContact"
    DiceContent _ ->
      genericSendMessage m "sendDice"
    VenueContent _ ->
      genericSendMessage m "sendVenue"
    LocationContent _ ->
      genericSendMessage m "sendLocation"
    PollContent _ ->
      genericSendMessage m "forwardMessage"
    UnsupportedContent _ ->
      genericSendMessage m "forwardMessage"

  chatIdOfMessage = pure . chatId


genericSendMessage :: Msg -> Text -> TelegramBot ()
genericSendMessage m u = do
    token' <- ("bot" <>) . extractToken <$> getToken

    logDebug "Sending Telegram response"

    _ <- runReq defaultHttpConfig
      $ req
          POST
          (https "api.telegram.org" /: token' /: u)
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


instance HasHelpMsg TelegramBot where
  getHelpMsg = asks helpMsg


instance HasRepetitions TelegramBot where
  getRepetitions ci = do
    rs <- liftIO . readTVarIO =<< asks repetitions
    dr <- asks defaultReps
    pure $ Map.findWithDefault dr ci rs

  updateRepetitions i ci = do
    rs <- asks repetitions
    liftIO $ atomically $ modifyTVar' rs (Map.insert ci i)

instance RepetitionsHandler TelegramBot where
  handleRepetitions msg@(Msg ci (CommandContent (RepeatCommand _))) = do
    rs <- asks repsCommandCalled
    liftIO $ atomically $ modifyTVar' rs (Set.insert ci)
    logInfo "User wants to update repetitions amount"
    pure msg

  handleRepetitions msg@(Msg ci (TextContent t)) = do
    trs <- asks repsCommandCalled
    rs <- liftIO $ readTVarIO trs

    case (Set.member ci rs, readMaybe $ Text.unpack t) of
      (True, Just i) -> do
        if i > 0 then do
          updateRepetitions i ci
          liftIO $ atomically $ modifyTVar' trs (Set.delete ci)
          logInfo "User repetitions was updated"
        else
          logWarning "User supplied wrong number of repetitions"
        pure $ Msg ci (TextContent "Repetitions amount was updated!")
      (True, Nothing) -> do
          logWarning "User supplied malformed number of repetitions"
          pure msg
      (False, _) ->
        pure msg

  handleRepetitions msg = pure msg

instance HasRepsQuestion TelegramBot where
  getRepsQuestion = asks repsQuestion

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
