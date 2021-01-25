{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Types.Config where

import           Bot
import           Bot.Telegram.Fetcher          (FetcherEnv (FetcherEnv))
import           Bot.Telegram.Sender           (SenderEnv (SenderEnv))
import           Bot.Telegram.Translator       (TranslatorEnv (TranslatorEnv))
import           Control.Concurrent            (myThreadId)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Exception             (throwIO, toException)
import           Data.Aeson                    hiding (Error)
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Logger


data TelegramConfig = TelegramConfig
  { token             :: Text
  , helpMessage       :: Text
  , defaultReps       :: Int
  , repsQuestion      :: Text
  , logLevel          :: Text
  , fetcherDelay      :: Int
  , translatorDelay   :: Int
  , senderDelay       :: Int
  , fetchersAmount    :: Int
  , translatorsAmount :: Int
  , sendersAmount     :: Int
  }


instance FromJSON TelegramConfig where
  parseJSON = withObject "TelegramConfig" $ \c -> TelegramConfig
    <$> c .: "token"
    <*> c .: "help_message"
    <*> c .: "default_repetitions"
    <*> c .: "repetitions_question"
    <*> c .: "log_level"
    <*> c .: "fetcher_delay"
    <*> c .: "translator_delay"
    <*> c .: "sender_delay"
    <*> c .: "fetchers_amount"
    <*> c .: "translators_amount"
    <*> c .: "senders_amount"


configToEnvs :: TelegramConfig -> IO (FetcherEnv, TranslatorEnv, SenderEnv)
configToEnvs (TelegramConfig t hm dr rq ll fd td sd _ _ _) = do
  offset <- newTVarIO 0
  uQueue <- newTQueueIO
  mQueue <- newTQueueIO
  repsAmount <- newTVarIO Map.empty
  repsCalled <- newTVarIO Set.empty
  threadId <- myThreadId
  ll' <- logLevel' ll
  let fEnv = FetcherEnv offset (Token t) ll' fd threadId uQueue
  let tEnv = TranslatorEnv uQueue mQueue ll' hm repsAmount dr repsCalled rq td
  let sEnv = SenderEnv (Token t) ll' threadId mQueue repsAmount dr sd
  pure (fEnv, tEnv, sEnv)
    where
      logLevel' "debug" = pure Debug
      logLevel' "info"  = pure Info
      logLevel' "warning" = pure Warning
      logLevel' "error"   = pure Error
      logLevel' _ = putStrLn "[Error] Unknown log level was specified in config file"
        >> error (Text.unpack $ ll <> " is unknown log level")
