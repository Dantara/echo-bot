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
import           Message
import           Network.HTTP.Req


newtype TelegramBot a = TelegramBot { unwrapBot :: ReaderT BotEnv IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader BotEnv
                   )


data ReceivedMessage = ReceivedMessage
  { chatId          :: Integer
  , messageId       :: Integer
  , text            :: Maybe Text
  , audio           :: Maybe FileInfo
  , document        :: Maybe FileInfo
  , photo           :: Maybe [FileInfo]
  , sticker         :: Maybe FileInfo
  , video           :: Maybe FileInfo
  , videoNote       :: Maybe FileInfo
  , voice           :: Maybe FileInfo
  , caption         :: Maybe Text
  , contact         :: Maybe Contact
  , dice            :: Maybe Dice
  , venue           :: Maybe Venue
  , messageLocation :: Maybe Location
  }

instance FromJSON ReceivedMessage where
  parseJSON = withObject "ReceivedMessage" $ \msg -> ReceivedMessage
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

data Contact = Contact
  { phoneNumber :: Text
  , firstName   :: Text
  , lastName    :: Maybe Text
  , userId      :: Maybe Integer
  , vcard       :: Maybe Text
  } deriving (Generic)

instance FromJSON Contact where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelToSnakeCase }

data Dice = Dice
  { diceEmoji :: Text
  , diceValue :: Int
  } deriving (Generic)

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


instance ProducerBot TelegramBot where
  data Update TelegramBot = Update
    { updateId :: Integer
    , message  :: Message
    } deriving Show

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

  updateToMessage = message

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
  deriving Show


instance FromJSON Updates where
  parseJSON = withObject "Updates" $ \us -> Updates
    <$> us .: "result"


runBot :: TelegramBot a -> BotEnv -> IO a
runBot app = runReaderT (unwrapBot app)


loopBot :: TelegramBot a -> BotEnv -> (BotEnv -> Int) -> IO ()
loopBot app env f = void $ forkIO $ forever $ do
  _ <- runBot app env
  threadDelay $ f env
