{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Bot where

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Data.Map.Strict               (Map)
import           Data.Set                      (Set)
import           Data.Text                     (Text)
import           Logger


class ( MonadIO m
      , HasToken m
      , HasOffset m
      , HasMessageQueue m
      , Logger m
      , HasHelpMsg m
      , RepetitionsHandler m
      ) => ProducerBot m where
  pullUpdates :: m [Update m]
  updateToMessage :: Update m -> m (Message m)
  offsetOfUpdate :: Update m -> Integer


class ( MonadIO m
      , HasMessageQueue m
      , HasToken m
      , HasRepetitions m
      , Logger m
      ) => ConsumerBot m where
  sendMessage :: Message m -> m ()
  chatIdOfMessage :: Message m -> m ChatId

class (Monad m) => BotTypes m where
  data Update m :: *
  type Message m :: *

data BotEnv ms = BotEnv
  { offset            :: TVar Integer
  , tMessages         :: TQueue ms
  , token             :: Token
  , logLevel          :: LogLevel
  , producerDelay     :: Int
  , consumerDelay     :: Int
  , helpMsg           :: Text
  , repetitions       :: TVar (Map ChatId Int)
  , defaultReps       :: Int
  , repsCommandCalled :: TVar (Set ChatId)
  , repsQuestion      :: Text
  }

data Command
  = HelpCommand Text
  | RepeatCommand Text

type ChatId = Integer

newtype Token = Token { extractToken :: Text }


class (Monad m) => HasOffset m where
  getOffset :: m Integer
  updateOffset :: Integer -> m ()


class (Monad m, BotTypes m) => HasMessageQueue m where
  pullMessage :: m (Maybe (Message m))
  pushMessage :: Message m -> m ()


class (Monad m) => HasToken m where
  getToken :: m Token

class (Monad m) => HasHelpMsg m where
  getHelpMsg :: m Text

class (Monad m) => HasRepetitions m where
  getRepetitions :: ChatId -> m Int
  updateRepetitions :: Int -> ChatId -> m ()

class (HasRepetitions m, Logger m) => RepetitionsHandler m where
  handleRepetitions :: Message m -> m (Message m)

class (HasRepetitions m) => HasRepsQuestion m where
  getRepsQuestion :: m Text
