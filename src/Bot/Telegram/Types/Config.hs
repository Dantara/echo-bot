{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.Telegram.Types.Config where

import           Bot                           (Token)
import           Bot.Telegram.Fetcher          (FetcherEnv (..))
import           Bot.Telegram.Sender           (SenderEnv (..))
import           Bot.Telegram.Translator       (TranslatorEnv (..))
import           Control.Concurrent            (myThreadId)
import           Control.Concurrent.STM.TQueue (newTQueueIO)
import           Control.Concurrent.STM.TVar   (newTVarIO)
import           Control.Exception             (throwIO, toException)
import           Data.Aeson                    (FromJSON (parseJSON),
                                                withObject, (.:))
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Logger                        (LogLevel)


data TelegramConfig = TelegramConfig
  { token             :: Token
  , helpMsg           :: Text
  , defaultReps       :: Int
  , repsQuestion      :: Text
  , logLevel          :: LogLevel
  , fetcherDelay      :: Int
  , translatorDelay   :: Int
  , senderDelay       :: Int
  , fetchersAmount    :: Int
  , translatorsAmount :: Int
  , sendersAmount     :: Int
  } deriving (Eq, Show)


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
configToEnvs TelegramConfig {..} = do
  offset <- newTVarIO 0
  tUpdates <- newTQueueIO
  tMessages <- newTQueueIO
  repetitions <- newTVarIO Map.empty
  repsCommandCalled <- newTVarIO Set.empty
  mainThreadId <- myThreadId
  pure (FetcherEnv {..}, TranslatorEnv {..}, SenderEnv {..})
