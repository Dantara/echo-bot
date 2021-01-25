{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}

module Bot.Telegram where

import           Bot.Telegram.Fetcher      (loopFetcher)
import           Bot.Telegram.Sender       (loopSender)
import           Bot.Telegram.Translator   (loopTranslator)
import           Bot.Telegram.Types.Config (TelegramConfig (..), configToEnvs)
import           Control.Monad             (replicateM_)
import           Logic                     (fetcher, sender, translator)


runBot :: TelegramConfig -> IO ()
runBot cfg = do
  (fEnv, tEnv, sEnv) <- configToEnvs cfg
  replicateM_ (fetchersAmount cfg) (loopFetcher fetcher fEnv)
  replicateM_ (translatorsAmount cfg) (loopTranslator translator tEnv)
  replicateM_ (sendersAmount cfg) (loopSender sender sEnv)
