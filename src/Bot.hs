{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot where

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Data.Text                     (Text)
import           Logger
import           Message


class (MonadReader BotEnv m, MonadIO m) => MonadBot m where
  getMessage :: m (Maybe Message)
  sendMessage :: Message -> m ()


data BotEnv = BotEnv
  {
    offset    :: TVar Integer
  , tMessages :: TQueue Message
  , token     :: Token
  , logLevel  :: LogLevel
  }


instance HasLogLevel BotEnv where
  getLogLevel = logLevel


newtype Token = Token Text


