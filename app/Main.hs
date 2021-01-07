{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Bot
import           Bot.Telegram
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Logger
import           Logic

defaultEnv :: IO (BotEnv Msg)
defaultEnv = do
  offset' <- newTVarIO 0
  queue' <- newTQueueIO
  rs <- newTVarIO Map.empty
  rcc <- newTVarIO Set.empty
  let token' = Token "1470862909:AAGZF-lhbKci7azP-NHsxiTCdGJ4flHlTDo"
  let logLevel' = Info
  let producerDelay' = 1000000
  let consumerDelay' = 1000000
  let helpMsg' = "Arbitrary help message"
  tId <- myThreadId
  pure $ BotEnv
    offset'
    queue'
    token'
    logLevel'
    producerDelay'
    consumerDelay'
    helpMsg'
    rs
    1
    rcc
    "How many times repeat?"
    tId



main :: IO ()
main = do
  env <- defaultEnv
  loopBot producer env producerDelay
  loopBot consumer env consumerDelay
  _ <- forever (threadDelay 1000000)
  pure ()
