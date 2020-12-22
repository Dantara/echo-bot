{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Bot.Telegram where

import           Bot
import           Control.Monad.IO.Class
import           Control.Monad.Reader
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


instance MonadBot TelegramBot where
  getMessage = return Nothing
  sendMessage _ = pure ()


instance Logger TelegramBot where
  log = logSTD


pullUpdates :: IO ()
pullUpdates = do
  let payload =
        object
          [ "offset" .= (0 :: Int)
          ]
  r <- runReq defaultHttpConfig
    $ req
        POST
        (https "api.telegram.org" /: token' /: "getUpdates")
        (ReqBodyJson payload)
        jsonResponse
        mempty
  liftIO $ print (responseBody r :: Updates)
    where
      token' = "bot" <> "1470862909:AAGZF-lhbKci7azP-NHsxiTCdGJ4flHlTDo"


data Update = Update
  { updateId :: Integer
  , message  :: Message
  } deriving Show


instance FromJSON Update where
  parseJSON = withObject "Update" $ \upd -> Update
    <$> upd .: "update_id"
    <*> upd .: "message"


newtype Updates = Updates [Update]
  deriving Show


instance FromJSON Updates where
  parseJSON = withObject "Updates" $ \us -> Updates
    <$> us .: "result"
