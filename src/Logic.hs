{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Logic where

import           Bot
import           Control.Monad
import           Logger


producer :: (ProducerBot m, Logger m) => m ()
producer = pullUpdates >>= mapM_ proceedUpdate
  where
    proceedUpdate u = do
      updateToMessage u
        >>= handleRepetitions
        >>= pushMessage
      updateOffset $ offsetOfUpdate u


consumer :: (ConsumerBot m, Logger m) => m ()
consumer = do
  maybeMsg <- pullMessage
  case maybeMsg of
    (Just m) -> do
      rs <- chatIdOfMessage m >>= getRepetitions
      replicateM_ rs (sendMessage m)
    Nothing ->
      logDebug "Consumer gets no updates"
