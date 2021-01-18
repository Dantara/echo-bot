{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}

module Bot.VK where

import           Bot               (BotPart (..))
import           Bot.VK.Fetcher    (FetcherEnv, loopFetcher)
import           Bot.VK.Sender     (SenderEnv, loopSender)
import           Bot.VK.Translator (TranslatorEnv, loopTranslator)
import           Logic             (fetcher, sender, translator)


runBot :: [BotPart FetcherEnv TranslatorEnv SenderEnv] -> IO ()
runBot = mapM_ runner
  where
    runner (Fetcher env)    = loopFetcher fetcher env
    runner (Translator env) = loopTranslator translator env
    runner (Sender env)     = loopSender sender env
