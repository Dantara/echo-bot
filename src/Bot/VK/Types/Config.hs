{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types.Config where

import           Bot
import           Bot.VK.Fetcher                (FetcherEnv (FetcherEnv))
import           Bot.VK.Sender                 (SenderEnv (SenderEnv))
import           Bot.VK.Translator             (TranslatorEnv (TranslatorEnv))
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


apiVersion :: Text
apiVersion = "5.126"


data VKConfig = VKConfig
  { accessToken     :: Text
  , groupId         :: Text
  , fetcherTimeout  :: Int
  , helpMessage     :: Text
  , defaultReps     :: Int
  , repsQuestion    :: Text
  , logLevel        :: Text
  , fetcherDelay    :: Int
  , translatorDelay :: Int
  , senderDelay     :: Int
  }


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


configToEnvs :: VKConfig -> IO (FetcherEnv, TranslatorEnv, SenderEnv)
configToEnvs (VKConfig at gi ft hm dr rq ll fd td sd) = do
    uQueue <- newTQueueIO
    mQueue <- newTQueueIO
    repsAmount <- newTVarIO Map.empty
    repsCalled <- newTVarIO Set.empty
    longPollServer <- newTVarIO Nothing
    threadId <- myThreadId
    ll' <- logLevel' ll
    let fEnv = FetcherEnv (Token at) gi ll' fd ft threadId longPollServer uQueue apiVersion
    let tEnv = TranslatorEnv uQueue mQueue ll' hm repsAmount dr repsCalled rq td
    let sEnv = SenderEnv (Token at) ll' threadId mQueue repsAmount dr sd apiVersion
    pure (fEnv, tEnv, sEnv)
  where
    logLevel' "debug"   = pure Debug
    logLevel' "info"    = pure Info
    logLevel' "warning" = pure Warning
    logLevel' "error"   = pure Error
    logLevel' _ = putStrLn "[Error] Unknown log level was specified in config file"
      >> error (Text.unpack $ ll <> " is unknown log level")
