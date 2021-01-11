{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Bot.VK.Fetcher where

import           Bot
import           Bot.VK.Fetcher.LongPollServer
import           Control.Concurrent            (ThreadId, forkFinally,
                                                killThread, myThreadId,
                                                threadDelay, throwTo)
import           Control.Concurrent.STM.TVar   (TVar, readTVarIO, writeTVar)
import           Control.Exception             (throwIO)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader, ReaderT, asks,
                                                runReaderT)
import           Control.Monad.STM             (atomically)
import           Data.Text                     (Text)
import           Logger
import           Network.HTTP.Req


apiVersion :: Text
apiVersion = "5.126"


newtype FetcherM a = FetcherM { unwrapFetcherM :: ReaderT FetcherEnv IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader FetcherEnv
                   )


data FetcherEnv = FetcherEnv
  { offset         :: TVar Integer
  , token          :: Token
  , groupId        :: Text
  , logLevel       :: LogLevel
  , fetcherDelay   :: Int
  , mainThreadId   :: ThreadId
  , longPollServer :: TVar (Maybe LongPollServer)
  -- , tUpdates     :: TQueue Upd
  }


updateLongPollServer :: FetcherM ()
updateLongPollServer = do
  token' <- asks $ extractToken . token
  groupId' <- asks groupId

  let params = [ "groupId" =: groupId'
               , "access_token" =: token'
               , "v" =: apiVersion
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


instance Logger FetcherM where
  log = logSTD
  getLogLevel = asks logLevel


instance MonadHttp FetcherM where
  handleHttpException e = do
    logError "Error occured while fetching updates:"
    tId <- asks mainThreadId
    liftIO $ throwTo tId e
    liftIO $ throwIO e

  getHttpConfig = pure defaultHttpConfig
