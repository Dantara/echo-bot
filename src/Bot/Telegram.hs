{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Bot.Telegram where

import           Bot                       (loopBot)
import           Bot.Telegram.Fetcher      (FetcherM)
import           Bot.Telegram.Sender       (SenderM)
import           Bot.Telegram.Translator   (TranslatorM)
import           Bot.Telegram.Types.Config (TelegramConfig (..), configToEnvs)
import           Control.Monad             (replicateM_)
import           Logic                     (fetcher, sender, translator)


startBot :: TelegramConfig -> IO ()
startBot cfg = do
  (fEnv, tEnv, sEnv) <- configToEnvs cfg
  replicateM_ (fetchersAmount cfg) (loopBot @FetcherM fetcher fEnv)
  replicateM_ (translatorsAmount cfg) (loopBot @TranslatorM translator tEnv)
  replicateM_ (sendersAmount cfg) (loopBot @SenderM sender sEnv)
