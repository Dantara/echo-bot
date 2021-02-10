{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Bot.VK where

import           Bot                 (loopBot)
import           Bot.VK.Fetcher      (FetcherM)
import           Bot.VK.Sender       (SenderM)
import           Bot.VK.Translator   (TranslatorM)
import           Bot.VK.Types.Config (VKConfig (..), configToEnvs)
import           Control.Monad       (replicateM_)
import           Logic               (fetcher, sender, translator)


startBot :: VKConfig -> IO ()
startBot cfg = do
  (fEnv, tEnv, sEnv) <- configToEnvs cfg
  replicateM_ (fetchersAmount cfg) (loopBot @FetcherM fetcher fEnv)
  replicateM_ (translatorsAmount cfg) (loopBot @TranslatorM translator tEnv)
  replicateM_ (sendersAmount cfg) (loopBot @SenderM sender sEnv)
