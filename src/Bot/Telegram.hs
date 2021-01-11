{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}

module Bot.Telegram where

import           Bot                     (BotPart (..))
import           Bot.Telegram.Fetcher    (FetcherEnv, loopFetcher)
import           Bot.Telegram.Sender     (SenderEnv, loopSender)
import           Bot.Telegram.Translator (TranslatorEnv, loopTranslator)
import           Logic                   (fetcher, sender, translator)


runBot :: [BotPart FetcherEnv TranslatorEnv SenderEnv] -> IO ()
runBot = mapM_ runner
  where
    runner (Fetcher env)    = loopFetcher fetcher env
    runner (Translator env) = loopTranslator translator env
    runner (Sender env)     = loopSender sender env
