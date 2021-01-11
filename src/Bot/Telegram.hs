{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}

module Bot.Telegram where

import           Bot                     (BotPart (..))
import           Bot.Telegram.Fetcher    (FetcherEnv, runFetcher)
import           Bot.Telegram.Sender     (SenderEnv, runSender)
import           Bot.Telegram.Translator (TranslatorEnv, runTranslator)
import           Logic                   (fetcher, sender, translator)


runBot :: [BotPart FetcherEnv TranslatorEnv SenderEnv] -> IO ()
runBot = mapM_ runner
  where
    runner (Fetcher env)    = runFetcher fetcher env
    runner (Translator env) = runTranslator translator env
    runner (Sender env)     = runSender sender env
