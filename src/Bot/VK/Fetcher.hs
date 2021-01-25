{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.VK.Fetcher where

import           Bot
import           Bot.VK.Fetcher.LongPollServer
import           Bot.VK.Types.Updates
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
import           Data.Functor                  ((<&>))
import           Data.Text                     (Text)
import           Debug.Trace
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
  { token          :: Token
  , groupId        :: Text
  , logLevel       :: LogLevel
  , fetcherDelay   :: Int
  , fetcherTimeout :: Int
  , mainThreadId   :: ThreadId
  , longPollServer :: TVar (Maybe LongPollServer)
  , tUpdates       :: TQueue Upd
  , apiVersion     :: Text
  }


instance MonadFetcher FetcherM where
  fetchUpdates = do
    maybeLps <- liftIO . readTVarIO =<< asks longPollServer

    maybe
      (updateLongPollServer >>= proceedRequest)
      proceedRequest
      maybeLps
    where
       proceedRequest lps = do
         timeout <- asks fetcherTimeout

         let params = [ "act" =: ("a_check" :: Text)
                      , "key" =: key lps
                      , "ts" =: ts lps
                      , "wait" =: timeout
                      ]

         logDebug "Fetching updates from VK"

         r <- req
                GET
                (serverAddr lps)
                NoReqBody
                jsonResponse
                (mconcat params)

         pure $ updatesToUpds $ responseBody r

  offsetOfUpdate = pure . updateId


instance HasUpdateQueue FetcherM where
  type Update FetcherM = Upd


instance HasUpdQueueSTM FetcherM where
  getUpdQueue = asks tUpdates


instance Logger FetcherM where
  getLogLevel = asks logLevel


instance MonadHttp FetcherM where
  handleHttpException e = do
    logError "Error occured while fetching VK updates:"
    tId <- asks mainThreadId
    liftIO $ throwTo tId e
    liftIO $ throwIO e

  getHttpConfig = pure defaultHttpConfig


instance HasOffset FetcherM where
  getOffset = asks longPollServer
    >>= liftIO . readTVarIO
    >>= maybe
        (updateLongPollServer <&> ts)
        (pure . ts)

  updateOffset o = do
    maybeLps <- liftIO . readTVarIO =<< asks longPollServer
    maybe
      (updateLongPollServer >>= update)
      update
      maybeLps
    where
      update lps = do
        tLPS <- asks longPollServer
        liftIO
          $ atomically
          $ writeTVar tLPS (Just lps { ts = o })


updateLongPollServer :: FetcherM LongPollServer
updateLongPollServer = do
  token' <- asks $ extractToken . token
  groupId' <- asks groupId
  apiV <- asks apiVersion

  let params = [ "group_id" =: groupId'
               , "access_token" =: token'
               , "v" =: apiV
               ]

  logDebug "Getting LongPoll server for VK"

  r <- req
        GET
        (https "api.vk.com" /: "method" /: "groups.getLongPollServer")
        NoReqBody
        jsonResponse
        (mconcat params)

  tLPS <- asks longPollServer
  liftIO
    $ atomically
    $ writeTVar tLPS (Just $ responseBody r)

  pure $ responseBody r


instance MonadSleep FetcherM where
  sleep = liftIO . threadDelay =<< asks fetcherDelay


runFetcher :: FetcherM a -> FetcherEnv -> IO a
runFetcher app = runReaderT (unwrapFetcherM app)


loopFetcher :: FetcherM a -> FetcherEnv -> IO ()
loopFetcher app env = void $ forkFinally
  (forever $ runFetcher app env)
  (either (const $ myThreadId >>= killThread) (const $ pure ()))
