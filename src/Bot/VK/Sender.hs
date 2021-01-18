{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.VK.Sender where

import           Bot
import           Bot.VK.Types.Msg
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
import           Data.Text                     (Text)
import           Logger
import           Network.HTTP.Req


apiVersion :: Text
apiVersion = "5.126"


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
  }


instance MonadSender SenderM where
  sendMessage msg = do
    token' <- asks $ extractToken . token

    let params = [ "access_token" =: token'
                 , "user_id" =: userId msg
                 , "random_id" =: randomId msg
                 , "message" =: text msg
                 , "attachments" =: serializeAttachments (attachments msg)
                 , "v" =: apiVersion
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

  pullMessage = asks tMessages
    >>= liftIO . atomically . tryReadTQueue

  pushMessage m = asks tMessages >>= \q ->
    liftIO $ atomically $ writeTQueue q m


instance HasRepetitions SenderM where
  getRepetitions ci = do
    rs <- liftIO . readTVarIO =<< asks repetitions
    dr <- asks defaultReps
    pure $ Map.findWithDefault dr ci rs

  updateRepetitions i ci = do
    rs <- asks repetitions
    liftIO $ atomically $ modifyTVar' rs (Map.insert ci i)


instance Logger SenderM where
  log = logSTD
  getLogLevel = asks logLevel


instance MonadHttp SenderM where
  handleHttpException e = do
    logError "Error occured while sending message:"
    tId <- asks mainThreadId
    liftIO $ throwTo tId e
    liftIO $ throwIO e

  getHttpConfig = pure defaultHttpConfig


instance MonadSleep SenderM where
  sleep = liftIO . threadDelay =<< asks senderDelay


runSender :: SenderM a -> SenderEnv -> IO a
runSender app = runReaderT (unwrapSenderM app)


loopSender :: SenderM a -> SenderEnv -> IO ()
loopSender app env = void $ forkFinally
  (forever $ runSender app env)
  (either (const $ myThreadId >>= killThread) (const $ pure ()))
