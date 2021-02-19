{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.VK.Translator where

import           Bot
import           Bot.Shared.RepeatCommandHandler (handleMsgWithRepeatCommand,
                                                  handleMsgWithText)
import           Bot.VK.Types.Msg                (Msg (Msg, randomId, userId))
import           Bot.VK.Types.Shared             (Attachment (..))
import           Bot.VK.Types.Updates            (ReceivedMsg (ReceivedMsg),
                                                  Upd (receivedMessage))
import           Control.Concurrent              (forkFinally, killThread,
                                                  myThreadId, threadDelay)
import           Control.Concurrent.STM.TQueue   (TQueue, tryReadTQueue,
                                                  writeTQueue)
import           Control.Concurrent.STM.TVar     (TVar, modifyTVar', readTVarIO)
import           Control.Monad                   (forever, void)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader            (MonadReader, ReaderT, asks,
                                                  runReaderT)
import           Control.Monad.STM               (atomically)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Logger                          (LogLevel,
                                                  Logger (getLogLevel),
                                                  logDebug, logWarning)
import           System.Random.TF                (newTFGen)
import           System.Random.TF.Gen            (RandomGen (next))
import           Text.Read                       (readMaybe)


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
        logDebug "VK help command was translated"
        pure $ Msg fi ri hm [] (Just HelpCommand)

      receivedMsgToMsg (ReceivedMsg fi "/repeat" []) = do
        ri <- nextRandomNumber
        rs <- getRepetitions fi
        let firstLine = "Current repetitions amount is " <>
                        Text.pack (show rs) <> "\n"
        q <- asks repsQuestion
        logDebug "VK repeat command was translated"
        pure $ Msg fi ri (firstLine <> q) [] (Just RepeatCommand)

      receivedMsgToMsg (ReceivedMsg fi t as) = do
        ri <- nextRandomNumber
        logDebug "VK update was translated"
        handleUnknownAttachment as
        pure $ Msg fi ri t as Nothing

      nextRandomNumber = fromIntegral . fst . next <$> liftIO newTFGen

      handleUnknownAttachment as = if UnknownAttachment `elem` as
        then logWarning "Unknown attachment was received"
        else pure ()


instance HasRepCallsSetSTM TranslatorM where
  getTVarRepCallsSet = asks repsCommandCalled


instance HasRepeatCalls TranslatorM


instance RepetitionsHandler TranslatorM where
  handleRepeatCommand msg@(Msg ci _ _ _ (Just RepeatCommand))
    = handleMsgWithRepeatCommand msg ci

  handleRepeatCommand msg@(Msg ci ri t _ _)
    = handleMsgWithText msg ci t msgProd
    where
      msgProd t' = Msg ci ri t' [] Nothing

  repeatMessage msg = do
    rs <- getRepetitions $ userId msg
    pure $ zipWith updateMsg [0..] (replicate rs msg)
      where
        updateMsg n m = m { randomId = randomId m + n }


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
  getDefaultRepetitions = asks defaultReps


instance HasMapRepsSTM TranslatorM where
  getTVarMapReps = asks repetitions


instance MonadSleep TranslatorM where
  sleep = liftIO . threadDelay =<< asks translatorDelay


instance Runnable TranslatorM TranslatorEnv where
  runBot app = runReaderT (unwrapTranslatorM app)
