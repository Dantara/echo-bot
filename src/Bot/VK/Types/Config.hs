{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VK.Types.Config where

import           Bot                           (Token)
import           Bot.VK.Fetcher                (FetcherEnv (..))
import           Bot.VK.Sender                 (SenderEnv (..))
import           Bot.VK.Translator             (TranslatorEnv (..))
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


-- | Api version is hard coded,
-- because in any way we should change the code to support
-- updated api version
defaultApiVersion :: Text
defaultApiVersion = "5.125"


data VKConfig = VKConfig
  { token             :: Token
  , groupId           :: Text
  , fetcherTimeout    :: Int
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


instance FromJSON VKConfig where
  parseJSON = withObject "VKConfig" $ \c -> VKConfig
    <$> c .: "access_token"
    <*> c .: "group_id"
    <*> c .: "fetcher_timeout"
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


configToEnvs :: VKConfig -> IO (FetcherEnv, TranslatorEnv, SenderEnv)
configToEnvs VKConfig {..} = do
  tUpdates <- newTQueueIO
  tMessages <- newTQueueIO
  repetitions <- newTVarIO Map.empty
  repsCommandCalled <- newTVarIO Set.empty
  longPollServer <- newTVarIO Nothing
  mainThreadId <- myThreadId
  let apiVersion = defaultApiVersion
  let fEnv = FetcherEnv { .. }
  let tEnv = TranslatorEnv { .. }
  let sEnv = SenderEnv { .. }
  pure (fEnv, tEnv, sEnv)
