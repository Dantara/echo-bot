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
-- import           Data.Aeson.Types
import           Logger
import           Message
import           Network.HTTP.Req


newtype TelegramBot a = TelegramBot { unwrapBot :: ReaderT BotEnv IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader BotEnv
           )


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

