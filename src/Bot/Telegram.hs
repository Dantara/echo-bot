{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.Telegram where

import           Bot
import           Control.Monad.Reader
import           Logger


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
