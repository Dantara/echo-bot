{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Logic where

import           Bot
import           Logger


producer :: (ProducerBot m, Logger m) => m ()
producer = pullUpdates >>= mapM_ proceedUpdate
  where
    proceedUpdate u = do
      pushMessage =<< updateToMessage u
      updateOffset $ offsetOfUpdate u


consumer :: (ConsumerBot m, Logger m) => m ()
consumer = pullMessage >>= maybe (logDebug "Consumer gets no updates") sendMessage
