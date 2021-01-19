{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.Telegram.Fetcher where

import           Bot
import           Bot.Telegram.Types.Updates
import           Control.Concurrent            (ThreadId, forkFinally,
                                                killThread, myThreadId,
                                                threadDelay, throwTo)
import           Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue,
                                                writeTQueue)
import           Control.Concurrent.STM.TVar   (TVar, readTVarIO, writeTVar)
import           Control.Exception             (throwIO)
import           Control.Monad                 (forever, void)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader, ReaderT, asks,
                                                runReaderT)
import           Control.Monad.STM             (atomically)
import           Data.Aeson
import           Logger
import           Network.HTTP.Req


newtype FetcherM a = FetcherM { unwrapFetcherM :: ReaderT FetcherEnv IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader FetcherEnv
                   )


data FetcherEnv = FetcherEnv
  { offset       :: TVar Integer
  , token        :: Token
  , logLevel     :: LogLevel
  , fetcherDelay :: Int
  , mainThreadId :: ThreadId
  , tUpdates     :: TQueue Upd
  }


instance MonadFetcher FetcherM where
  fetchUpdates = do
    o <- liftIO . readTVarIO =<< asks offset

    let payload = object
          [ "offset" .= (o + 1)
          ]

    token' <- asks $ ("bot" <>) . extractToken . token

    logDebug "Fetching updates from Telegram"

    r <- req
          POST
          (https "api.telegram.org" /: token' /: "getUpdates")
          (ReqBodyJson payload)
          jsonResponse
          mempty

    pure $ extractUpdates $ responseBody r

  offsetOfUpdate = pure . updateId


instance HasUpdateQueue FetcherM where
  type Update FetcherM = Upd


instance HasUpdQueueSTM FetcherM where
  getUpdQueue = asks tUpdates


instance Logger FetcherM where
  getLogLevel = asks logLevel


instance MonadHttp FetcherM where
  handleHttpException e = do
    logError "Error occured while fetching updates:"
    tId <- asks mainThreadId
    liftIO $ throwTo tId e
    liftIO $ throwIO e

  getHttpConfig = pure defaultHttpConfig


instance HasOffset FetcherM where
  getOffset = liftIO . readTVarIO =<< asks offset

  updateOffset o = asks offset >>= \t ->
    liftIO $ atomically $ writeTVar t o


instance MonadSleep FetcherM where
  sleep = liftIO . threadDelay =<< asks fetcherDelay


runFetcher :: FetcherM a -> FetcherEnv -> IO a
runFetcher app = runReaderT (unwrapFetcherM app)


loopFetcher :: FetcherM a -> FetcherEnv -> IO ()
loopFetcher app env = void $ forkFinally
  (forever $ runFetcher app env)
  (either (const $ myThreadId >>= killThread) (const $ pure ()))
