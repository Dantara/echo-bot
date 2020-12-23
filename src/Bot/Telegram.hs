{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.Telegram where

import           Bot
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Aeson
import           Data.Aeson.Types
import           Logger
import           Message
import           Network.HTTP.Req


newtype TelegramBot x = TelegramBot (ReaderT BotEnv IO x)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader BotEnv
           )


instance ConsumerBot TelegramBot where
  sendMessage = liftIO . print


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
          [ "offset" .= o
          ]

    token' <- ("bot" <>) . extractToken <$> getToken

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


-- pullUpdates :: IO ()
-- pullUpdates = do
--   let payload =
--         object
--           [ "offset" .= (0 :: Int)
--           ]
--   r <- runReq defaultHttpConfig
--     $ req
--         POST
--         (https "api.telegram.org" /: token' /: "getUpdates")
--         (ReqBodyJson payload)
--         jsonResponse
--         mempty
--   liftIO $ print (responseBody r :: Updates)
--     where
--       token' = "bot" <> "1470862909:AAGZF-lhbKci7azP-NHsxiTCdGJ4flHlTDo"
