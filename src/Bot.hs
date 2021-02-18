{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Bot where

import           Control.Concurrent            (ThreadId, forkFinally,
                                                killThread, myThreadId)
import           Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue,
                                                writeTQueue)
import           Control.Concurrent.STM.TVar   (TVar, modifyTVar', readTVarIO)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.STM             (atomically)
import           Data.Aeson
import           Data.Functor                  ((<&>))
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
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
  pushUpdate msg = getUpdQueue >>= \q -> liftIO $ atomically $ writeTQueue q msg


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


class (MonadIO m) => HasRepCallsSetSTM m where
  getTVarRepCallsSet :: m (TVar (Set ChatId))


class (Monad m) => HasRepeatCalls m where
  addRepCall :: ChatId -> m ()
  default addRepCall :: (HasRepCallsSetSTM m) => ChatId -> m ()
  addRepCall ci = do
    rs <- getTVarRepCallsSet
    liftIO $ atomically $ modifyTVar' rs (Set.insert ci)

  removeRepCall :: ChatId -> m ()
  default removeRepCall :: (HasRepCallsSetSTM m) => ChatId -> m ()
  removeRepCall ci = do
    rs <- getTVarRepCallsSet
    liftIO $ atomically $ modifyTVar' rs (Set.delete ci)

  wasRepCalled :: ChatId -> m Bool
  default wasRepCalled :: (HasRepCallsSetSTM m) => ChatId -> m Bool
  wasRepCalled ci = Set.member ci
    <$> (liftIO . readTVarIO =<< getTVarRepCallsSet)


class (HasRepetitions m, Logger m) => RepetitionsHandler m where
  handleRepeatCommand :: Message m -> m (Message m)

  repeatMessage :: Message m -> m [Message m]


class (Monad m) => MonadSleep m where
  sleep :: m ()


class (Monad m) => HasMainThreadId m where
  getMainThreadId :: m ThreadId


class (MonadIO m) => Runnable m s | m -> s where
  runBot :: m a -> s -> IO a


data Command
  = HelpCommand
  | RepeatCommand
  deriving (Eq)


type ChatId = Integer


newtype Token = Token { extractToken :: Text }
  deriving (Eq, Show)


instance FromJSON Token where
  parseJSON = withText "Token" (pure . Token)


loopBot :: (Runnable m s) => m a -> s -> IO ()
loopBot app env = void $ forkFinally
  (forever $ runBot app env)
  (either (const $ myThreadId >>= killThread) (const $ pure ()))
