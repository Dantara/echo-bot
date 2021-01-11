{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.Telegram.Sender where

import           Bot
import           Bot.Telegram.Types.Msg        (Msg (..), MsgContent (..))
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


newtype SenderM a = SenderM { unwrapSenderM :: ReaderT SenderEnv IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader SenderEnv
                   )


data SenderEnv = SenderEnv
  { offset       :: TVar Integer
  , token        :: Token
  , logLevel     :: LogLevel
  , mainThreadId :: ThreadId
  , tMessages    :: TQueue Msg
  , repetitions  :: TVar (Map ChatId Int)
  , defaultReps  :: Int
  , senderDelay  :: Int
  }


instance MonadSender SenderM where
  sendMessage m = case msgContent m of
    CommandContent _ ->
      genericSendMessage m "sendMessage"
    TextContent _ ->
      genericSendMessage m "sendMessage"
    AudioContent _ ->
      genericSendMessage m "sendAudio"
    DocumentContent _ _ ->
      genericSendMessage m "sendDocument"
    PhotoContent _ _ ->
      genericSendMessage m "sendPhoto"
    StickerContent _ ->
      genericSendMessage m "sendSticker"
    VideoContent _ _ ->
      genericSendMessage m "sendVideo"
    VideoNoteContent _ ->
      genericSendMessage m "sendVideoNote"
    VoiceContent _ ->
      genericSendMessage m "sendVoice"
    ContactContent _ ->
      genericSendMessage m "sendContact"
    DiceContent _ ->
      genericSendMessage m "sendDice"
    VenueContent _ ->
      genericSendMessage m "sendVenue"
    LocationContent _ ->
      genericSendMessage m "sendLocation"
    UnsupportedContent _ ->
      genericSendMessage m "forwardMessage"

  chatIdOfMessage = pure . chatId


genericSendMessage :: Msg -> Text -> SenderM ()
genericSendMessage m u = do
    token' <- asks $ ("bot" <>) . extractToken . token

    logDebug "Sending Telegram response"

    void $ req
             POST
             (https "api.telegram.org" /: token' /: u)
             (ReqBodyJson m)
             ignoreResponse
             mempty


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
