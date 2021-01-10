{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.Telegram.Fetcher where

import           Bot
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue (TQueue, readTQueue, writeTQueue)
import           Control.Concurrent.STM.TVar   (TVar, readTVarIO)
import           Control.Exception             (throwIO)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader, ReaderT, asks)
import           Control.Monad.STM             (atomically)
import           Data.Aeson
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Logger
import           Network.HTTP.Req
-- import           Text.Read
import           Bot.Telegram.Fetcher.Updates
import           Bot.Telegram.SharedTypes      (Contact, Dice, FileInfo,
                                                Location, Msg (..),
                                                MsgContent (..), Venue)


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


instance HasUpdateQueue FetcherM where
  type Update FetcherM = Upd

  pullUpdate = asks tUpdates
    >>= liftIO . atomically . readTQueue

  pushUpdate m = asks tUpdates >>= \q ->
    liftIO $ atomically $ writeTQueue q m


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
