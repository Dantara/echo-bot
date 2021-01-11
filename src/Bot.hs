{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Bot where

import           Control.Monad.Reader
import           Data.Text            (Text)
import           Logger


data BotPart a b c
  = Fetcher a
  | Translator b
  | Sender c


class (Monad m, Logger m) => MonadFetcher m where
  fetchUpdates :: m [Update m]
  offsetOfUpdate :: Update m -> m Integer


class (Monad m, Logger m) => MonadTranslator m where
  updateToMessage :: Update m -> m (Message m)


class (MonadIO m, Logger m) => MonadSender m where
  sendMessage :: Message m -> m ()
  chatIdOfMessage :: Message m -> m ChatId


class (Monad m) => HasOffset m where
  getOffset :: m Integer
  updateOffset :: Integer -> m ()


class (Monad m) => HasUpdateQueue m where
  type Update m :: *
  pushUpdate :: Update m -> m ()
  pullUpdate :: m (Update m)


class (Monad m) => HasMessageQueue m where
  type Message m :: *
  pushMessage :: Message m -> m ()
  pullMessage :: m (Message m)


class (Monad m) => HasRepetitions m where
  getRepetitions :: ChatId -> m Int
  updateRepetitions :: Int -> ChatId -> m ()


class (HasRepetitions m, Logger m) => RepetitionsHandler m where
  handleRepetitions :: Message m -> m (Message m)


data Command
  = HelpCommand Text
  | RepeatCommand Text


type ChatId = Integer


newtype Token = Token { extractToken :: Text }


