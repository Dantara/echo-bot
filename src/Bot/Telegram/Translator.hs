{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.Telegram.Translator where

import           Bot
import           Bot.Shared.RepeatCommandHandler
import           Bot.Telegram.Types.Msg
import           Bot.Telegram.Types.Updates
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
import           Logger
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
  updateToMessage = receivedMsgToMsg . message


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


instance HasRepCallsSetSTM TranslatorM where
  getTVarRepCallsSet = asks repsCommandCalled


instance HasRepeatCalls TranslatorM


instance RepetitionsHandler TranslatorM where
  handleRepeatCommand msg@(Msg ci (CommandContent RepeatCommand _))
    = handleMsgWithRepeatCommand msg ci

  handleRepeatCommand msg@(Msg ci (TextContent t))
    = handleMsgWithText msg ci t msgProd
    where
      msgProd t' = Msg ci (TextContent t')

  handleRepeatCommand msg = pure msg

  repeatMessage msg = do
    rs <- getRepetitions $ chatId msg
    pure $ replicate rs msg


instance Runnable TranslatorM TranslatorEnv where
  runBot app = runReaderT (unwrapTranslatorM app)


receivedMsgToMsg :: ReceivedMsg -> TranslatorM Msg
receivedMsgToMsg (ReceivedMsg ci _ (Just "/help") _ _ _ _ _ _ _ _ _ _ _ _)
  = asks helpMsg >>= \h -> pure $ Msg ci (CommandContent HelpCommand h)

receivedMsgToMsg (ReceivedMsg ci _ (Just "/repeat") _ _ _ _ _ _ _ _ _ _ _ _) = do
  q <- asks repsQuestion
  rs <- getRepetitions ci
  let firstLine = "Current repetitions amount is " <>
                  Text.pack (show rs) <> "\n"
  pure $ Msg ci (CommandContent RepeatCommand (firstLine <> q))

receivedMsgToMsg (ReceivedMsg ci _ (Just t) _ _ _ _ _ _ _ _ _ _ _ _)
  = pure $ Msg ci (TextContent t)

receivedMsgToMsg (ReceivedMsg ci _ _ (Just f) _ _ _ _ _ _ _ _ _ _ _)
  = pure $ Msg ci (AudioContent f)

receivedMsgToMsg (ReceivedMsg ci _ _ _ (Just f) _ _ _ _ _ c _ _ _ _)
  = pure $ Msg ci (DocumentContent f c)

receivedMsgToMsg (ReceivedMsg ci _ _ _ _ (Just fs) _ _ _ _ c _ _ _ _)
  = pure $ Msg ci (PhotoContent (last fs) c)

receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ (Just f) _ _ _ _ _ _ _ _)
  = pure $ Msg ci (StickerContent f)

receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ (Just f) _ _ c _ _ _ _)
  = pure $ Msg ci (VideoContent f c)

receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ (Just f) _ _ _ _ _ _)
  = pure $ Msg ci (VideoNoteContent f)

receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ (Just f) _ _ _ _ _)
  = pure $ Msg ci (VoiceContent f)

receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ (Just c) _ _ _)
  = pure $ Msg ci (ContactContent c)

receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ _ (Just d) _ _)
  = pure $ Msg ci (DiceContent d)

receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ _ _ (Just v) _)
  = pure $ Msg ci (VenueContent v)

receivedMsgToMsg (ReceivedMsg ci _ _ _ _ _ _ _ _ _ _ _ _ _ (Just l))
  = pure $ Msg ci (LocationContent l)

receivedMsgToMsg (ReceivedMsg ci i _ _ _ _ _ _ _ _ _ _ _ _ _)
  = pure $ Msg ci (UnsupportedContent i)
