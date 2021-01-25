{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}

module Bot.VK where

import           Bot.VK.Fetcher      (loopFetcher)
import           Bot.VK.Sender       (loopSender)
import           Bot.VK.Translator   (loopTranslator)
import           Bot.VK.Types.Config (VKConfig (..), configToEnvs)
import           Control.Monad       (replicateM_)
import           Logic               (fetcher, sender, translator)


runBot :: VKConfig -> IO ()
runBot cfg = do
  (fEnv, tEnv, sEnv) <- configToEnvs cfg
  replicateM_ (fetchersAmount cfg) (loopFetcher fetcher fEnv)
  replicateM_ (translatorsAmount cfg) (loopTranslator translator tEnv)
  replicateM_ (sendersAmount cfg) (loopSender sender sEnv)
