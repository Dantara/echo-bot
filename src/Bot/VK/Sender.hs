{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.VK.Sender where

import           Bot
import           Bot.Shared                    (defaultHttpExceptionHander)
import           Bot.VK.Sender.Keyboard        (getKeyboard)
import           Bot.VK.Types.Msg              (Msg (attachments, command, randomId, text, userId),
                                                serializeAttachments)
import           Bot.VK.Types.Shared           (Attachment (..))
import           Control.Concurrent            (ThreadId, forkFinally,
                                                killThread, myThreadId,
                                                threadDelay, throwTo)
import           Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue,
                                                writeTQueue)
import           Control.Concurrent.STM.TVar   (TVar, modifyTVar', readTVarIO)
import           Control.Exception             (throwIO)
import           Control.Monad                 (forever, void)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader, ReaderT, asks,
                                                runReaderT)
import           Control.Monad.STM             (atomically)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Logger                        (LogLevel, Logger (getLogLevel),
                                                logDebug)
import           Network.HTTP.Req              (GET (GET), MonadHttp (..),
                                                NoReqBody (NoReqBody),
                                                defaultHttpConfig, https,
                                                ignoreResponse, req, (/:), (=:))


newtype SenderM a = SenderM { unwrapSenderM :: ReaderT SenderEnv IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader SenderEnv
                   )


data SenderEnv = SenderEnv
  { token        :: Token
  , logLevel     :: LogLevel
  , mainThreadId :: ThreadId
  , tMessages    :: TQueue Msg
  , repetitions  :: TVar (Map ChatId Int)
  , defaultReps  :: Int
  , senderDelay  :: Int
  , apiVersion   :: Text
  }


instance MonadSender SenderM where
  sendMessage msg = do
    token' <- asks $ extractToken . token
    apiV <- asks apiVersion

    let params = [ "access_token" =: token'
                 , "user_id" =: userId msg
                 , "message" =: text msg
                 , "random_id" =: randomId msg
                 , "attachment" =: serializeAttachments (attachments msg)
                 , "v" =: apiV
                 ] <>
                 [ "sticker_id" =: s
                 | s <- mapMaybe
                   (\case (Sticker x) -> Just x; _ -> Nothing)
                   (attachments msg)
                 ] <>
                 [ "keyboard" =: ($(getKeyboard) :: Text)
                 | command msg == Just RepeatCommand
                 ]

    logDebug "Sending message to VK"

    void $ req
             GET
             (https "api.vk.com" /: "method" /: "messages.send")
             NoReqBody
             ignoreResponse
             (mconcat params)

  chatIdOfMessage = pure . userId


instance HasMessageQueue SenderM where
  type Message SenderM = Msg


instance HasMsgQueueSTM SenderM where
  getMsgQueue = asks tMessages


instance HasRepetitions SenderM where
  getDefaultRepetitions = asks defaultReps


instance HasMapRepsSTM SenderM where
  getTVarMapReps = asks repetitions


instance Logger SenderM where
  getLogLevel = asks logLevel


instance HasMainThreadId SenderM where
  getMainThreadId = asks mainThreadId


instance MonadHttp SenderM where
  handleHttpException =
    defaultHttpExceptionHander
      "Error occured while sending VK message"

  getHttpConfig = pure defaultHttpConfig


instance MonadSleep SenderM where
  sleep = liftIO . threadDelay =<< asks senderDelay


instance Runnable SenderM SenderEnv where
  runBot app = runReaderT (unwrapSenderM app)
