{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Bot
import           Bot.Telegram.Fetcher
import           Bot.Telegram.Sender
import           Bot.Telegram.Translator
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Logger


defaultEnvs :: IO (FetcherEnv, TranslatorEnv, SenderEnv)
defaultEnvs = do
  offset' <- newTVarIO 0
  uQueue' <- newTQueueIO
  mQueue' <- newTQueueIO
  rs <- newTVarIO Map.empty
  rcc <- newTVarIO Set.empty
  let token' = Token "1470862909:AAGZF-lhbKci7azP-NHsxiTCdGJ4flHlTDo"
  let logLevel' = Debug
  let delay' = 1000000
  let helpMsg' = "Arbitrary help message"
  let repsQ' = "How many times repeat?"
  let drs = 1
  tId <- myThreadId

  let fEnv = FetcherEnv offset' token' logLevel' delay' tId uQueue'
  let tEnv = TranslatorEnv uQueue' mQueue' logLevel' helpMsg' rs drs rcc repsQ' delay'
  let sEnv = SenderEnv offset' token' logLevel' tId mQueue' rs drs delay'

  pure (fEnv, tEnv, sEnv)
