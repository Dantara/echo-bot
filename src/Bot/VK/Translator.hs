{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.VK.Translator where

import           Bot
import           Bot.VK.Types.Msg
import           Bot.VK.Types.Updates
import           Control.Concurrent            (forkFinally, killThread,
                                                myThreadId, threadDelay)
import           Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue,
                                                writeTQueue)
import           Control.Concurrent.STM.TVar   (TVar, modifyTVar', readTVarIO)
import           Control.Monad                 (forever, void)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader, ReaderT, asks,
                                                runReaderT)
import           Control.Monad.STM             (atomically)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Logger
import           System.Random.TF
import           System.Random.TF.Gen
import           Text.Read                     (readMaybe)


newtype TranslatorM a = TranslatorM { unwrapTranslatorM :: ReaderT TranslatorEnv IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader TranslatorEnv
           )


data TranslatorEnv = TranslatorEnv
  { tUpdates          :: TQueue Upd
  , tMessages         :: TQueue Msg
  , logLevel          :: LogLevel
  , helpMsg           :: Text
  , repetitions       :: TVar (Map ChatId Int)
  , defaultReps       :: Int
  , repsCommandCalled :: TVar (Set ChatId)
  , repsQuestion      :: Text
  , translatorDelay   :: Int
  }


instance MonadTranslator TranslatorM where
  updateToMessage = receivedMsgToMsg . receivedMessage
    where
      receivedMsgToMsg (ReceivedMsg fi "/help" []) = do
        ri <- nextRandomNumber
        hm <- asks helpMsg
        pure $ Msg fi ri hm [] (Just HelpCommand)

      receivedMsgToMsg (ReceivedMsg fi "/repeat" []) = do
        ri <- nextRandomNumber
        rs <- getRepetitions fi
        let firstLine = "Current repetitions amount is " <>
                        Text.pack (show rs) <> "\n"
        q <- asks repsQuestion
        pure $ Msg fi ri (firstLine <> q) [] (Just HelpCommand)

      receivedMsgToMsg (ReceivedMsg fi t as) = do
        ri <- nextRandomNumber
        pure $ Msg fi ri t as Nothing

      nextRandomNumber = fromIntegral . fst . next <$> liftIO newTFGen


instance HasUpdateQueue TranslatorM where
  type Update TranslatorM = Upd


instance HasMessageQueue TranslatorM where
  type Message TranslatorM = Msg


instance HasUpdQueueSTM TranslatorM where
  getUpdQueue = asks tUpdates


instance HasMsgQueueSTM TranslatorM where
  getMsgQueue = asks tMessages


instance Logger TranslatorM where
  getLogLevel = asks logLevel


instance HasRepetitions TranslatorM where
  getRepetitions ci = do
    rs <- liftIO . readTVarIO =<< asks repetitions
    dr <- asks defaultReps
    pure $ Map.findWithDefault dr ci rs

  updateRepetitions i ci = do
    rs <- asks repetitions
    liftIO $ atomically $ modifyTVar' rs (Map.insert ci i)


instance RepetitionsHandler TranslatorM where
  handleRepetitions msg@(Msg ci _ _ _ (Just RepeatCommand)) = do
    rs <- asks repsCommandCalled
    liftIO $ atomically $ modifyTVar' rs (Set.insert ci)
    logInfo "User wants to update repetitions amount"
    pure msg

  handleRepetitions msg@(Msg ci ri t _ _) = do
    trs <- asks repsCommandCalled
    rs <- liftIO $ readTVarIO trs

    case (Set.member ci rs, readMaybe $ Text.unpack t) of
      (True, Just i) -> do
        if i > 0 then do
          updateRepetitions i ci
          liftIO $ atomically $ modifyTVar' trs (Set.delete ci)
          logInfo "User repetitions was updated"
        else
          logWarning "User supplied wrong number of repetitions"
        pure $ Msg ci ri "Repetitions amount was updated!" [] Nothing
      (True, Nothing) -> do
          logWarning "User supplied malformed number of repetitions"
          pure msg
      (False, _) ->
        pure msg


instance MonadSleep TranslatorM where
  sleep = liftIO . threadDelay =<< asks translatorDelay


runTranslator :: TranslatorM a -> TranslatorEnv -> IO a
runTranslator app = runReaderT (unwrapTranslatorM app)


loopTranslator :: TranslatorM a -> TranslatorEnv -> IO ()
loopTranslator app env = void $ forkFinally
  (forever $ runTranslator app env)
  (either (const $ myThreadId >>= killThread) (const $ pure ()))
