{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Bot where

import           Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue,
                                                writeTQueue)
import           Control.Concurrent.STM.TVar   (TVar, modifyTVar', readTVarIO)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.STM             (atomically)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import           Logger


class (Monad m, Logger m) => MonadFetcher m where
  fetchUpdates :: m [Update m]
  offsetOfUpdate :: Update m -> m Integer


class (Monad m, Logger m) => MonadTranslator m where
  updateToMessage :: Update m -> m (Message m)


class (Monad m, Logger m) => MonadSender m where
  sendMessage :: Message m -> m ()
  chatIdOfMessage :: Message m -> m ChatId


class (Monad m) => HasOffset m where
  getOffset :: m Integer
  updateOffset :: Integer -> m ()


class (MonadIO m) => HasUpdQueueSTM m where
  getUpdQueue :: m (TQueue (Update m))


class (Monad m) => HasUpdateQueue m where
  type Update m :: *

  pullUpdate :: m (Maybe (Update m))
  default pullUpdate :: (HasUpdQueueSTM m) => m (Maybe (Update m))
  pullUpdate = getUpdQueue
    >>= liftIO . atomically . tryReadTQueue

  pushUpdate :: Update m -> m ()
  default pushUpdate :: (HasUpdQueueSTM m) => Update m -> m ()
  pushUpdate msg = getUpdQueue >>= \q ->
    liftIO $ atomically $ writeTQueue q msg


class (MonadIO m) => HasMsgQueueSTM m where
  getMsgQueue :: m (TQueue (Message m))


class (Monad m) => HasMessageQueue m where
  type Message m :: *

  pullMessage :: m (Maybe (Message m))
  default pullMessage :: (HasMsgQueueSTM m) => m (Maybe (Message m))
  pullMessage = getMsgQueue
    >>= liftIO . atomically . tryReadTQueue

  pushMessage :: Message m -> m ()
  default pushMessage :: (HasMsgQueueSTM m) => Message m -> m ()
  pushMessage msg = getMsgQueue >>= \q ->
    liftIO $ atomically $ writeTQueue q msg


class (MonadIO m) => HasMapRepsSTM m where
  getTVarMapReps :: m (TVar (Map ChatId Int))


class (Monad m) => HasRepetitions m where
  getDefaultRepetitions :: m Int

  getRepetitions :: ChatId -> m Int
  default getRepetitions :: HasMapRepsSTM m => ChatId -> m Int
  getRepetitions ci = do
    rs <- liftIO . readTVarIO =<< getTVarMapReps
    dr <- getDefaultRepetitions
    pure $ Map.findWithDefault dr ci rs

  updateRepetitions :: Int -> ChatId -> m ()
  default updateRepetitions :: HasMapRepsSTM m => Int -> ChatId -> m ()
  updateRepetitions i ci = do
    rs <- getTVarMapReps
    liftIO $ atomically $ modifyTVar' rs (Map.insert ci i)


class (HasRepetitions m, Logger m) => RepetitionsHandler m where
  handleRepeatCommand :: Message m -> m (Message m)

  repeatMessage :: Message m -> m [Message m]


class (Monad m) => MonadSleep m where
  sleep :: m ()


data Command
  = HelpCommand
  | RepeatCommand
  deriving (Eq)


type ChatId = Integer


newtype Token = Token { extractToken :: Text }


