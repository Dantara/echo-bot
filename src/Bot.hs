{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Bot where

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Data.Text                     (Text)
import           Message


class (MonadReader BotEnv m, MonadIO m) => BotMonad m where
  getMessage :: m (Maybe Message)
  sendMessage :: Message -> m ()


newtype TelegramBot x = TelegramBot (ReaderT BotEnv IO x)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader BotEnv
           )

data BotEnv = BotEnv
  {
    offset    :: TVar Integer
  , tMessages :: TQueue Message
  , token     :: Token
  }

newtype Token = Token Text


instance BotMonad TelegramBot where
  getMessage = return Nothing
  sendMessage _ = pure ()


