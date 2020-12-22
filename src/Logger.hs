{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger where

import           Control.Monad.Reader
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Prelude              hiding (log)


data STDLogger -- ^ Logger for standart I/O
data IdentityLogger -- ^ Mocking (disabling) logging


data LogLevel
  = Debug    -- ^ Debug messages
  | Info     -- ^ Notable information that requires no immediate action.
  | Warning  -- ^ Something is probably wrong, and we should investigate.
  | Error    -- ^ Something is wrong and immediate action is required.
  deriving (Eq, Ord, Show)


class HasLogLevel env where
  getLogLevel :: env -> LogLevel


class Logger m where
  log :: (MonadReader env m, HasLogLevel env) => LogLevel -> Text -> m ()


-- | Helpers methods for logging
logDebug, logInfo, logWarning, logError
  :: (Logger m, MonadReader env m, HasLogLevel env) => Text -> m ()
logDebug = log Debug
logInfo = log Info
logWarning = log Warning
logError = log Error


-- | Log to stdio
logSTD :: (MonadReader env m, HasLogLevel env, MonadIO m) => LogLevel -> Text -> m ()
logSTD l t = asks getLogLevel >>= \l' ->
  if l > l' then liftIO (T.putStrLn msg) else pure ()
    where
      msg = "[" <> T.pack (show l) <> "] " <> t


-- | Mock (drop) log message
logIdentity :: Applicative m => LogLevel -> Text -> m ()
logIdentity _ _ = pure ()
