{-# LANGUAGE OverloadedStrings #-}

module Bot.Shared where

import           Bot                    (HasMainThreadId (..))
import           Control.Concurrent     (throwTo)
import           Control.Exception      (throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           Logger                 (Logger, logError)
import           Network.HTTP.Req       (HttpException)

defaultHttpExceptionHander ::
  (MonadIO m, HasMainThreadId m, Logger m) =>
  Text ->
  HttpException ->
  m a
defaultHttpExceptionHander msg e = do
  logError msg
  tId <- getMainThreadId
  liftIO $ throwTo tId e
  liftIO $ throwIO e
