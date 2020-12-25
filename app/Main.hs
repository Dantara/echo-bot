{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Bot
import           Bot.Telegram
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Logger
import           Logic

defaultEnv :: IO BotEnv
defaultEnv = do
  offset' <- newTVarIO 0
  queue' <- newTQueueIO
  let token' = Token "1470862909:AAGZF-lhbKci7azP-NHsxiTCdGJ4flHlTDo"
  let logLevel' = Debug
  let producerDelay' = 1000000
  let consumerDelay' = 1000000
  pure $ BotEnv offset' queue' token' logLevel' producerDelay' consumerDelay'


main :: IO ()
main = do
  env <- defaultEnv
  loopBot producer env producerDelay
  loopBot consumer env consumerDelay
  _ <- forever (threadDelay 1000000)
  pure ()
