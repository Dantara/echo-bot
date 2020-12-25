{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Bot where

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Data.Text                     (Text)
import           Logger
import           Message


class (MonadIO m, HasToken m, HasOffset m, HasMessageQueue m, Logger m) => ProducerBot m where
  data Update m :: *
  pullUpdates :: m [Update m]
  updateToMessage :: Update m -> Message
  offsetOfUpdate :: Update m -> Integer


class (MonadIO m, HasMessageQueue m, HasToken m, Logger m) => ConsumerBot m where
  sendMessage :: Message -> m ()


data BotEnv = BotEnv
  {
    offset        :: TVar Integer
  , tMessages     :: TQueue Message
  , token         :: Token
  , logLevel      :: LogLevel
  , producerDelay :: Int
  , consumerDelay :: Int
  }


newtype Token = Token { extractToken :: Text }


class (Monad m) => HasOffset m where
  getOffset :: m Integer
  updateOffset :: Integer -> m ()


class (Monad m) => HasMessageQueue m where
  pullMessage :: m (Maybe Message)
  pushMessage :: Message -> m ()


class (Monad m) => HasToken m where
  getToken :: m Token
