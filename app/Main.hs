{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Bot
import qualified Bot.Telegram       as Telegram
import           Config
import           Control.Concurrent
import           Control.Monad


main :: IO ()
main = do
  (fEnv, tEnv, sEnv) <- defaultEnvs
  Telegram.runBot [Fetcher fEnv, Translator tEnv, Sender sEnv]
  _ <- forever (threadDelay 1000000)
  pure ()
