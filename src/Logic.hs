{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Logic where

import           Bot
import           Control.Monad (mapM_, replicateM_)
import           Logger


fetcher :: (MonadFetcher m, HasUpdateQueue m, HasOffset m) => m ()
fetcher = fetchUpdates >>= mapM_ proceedUpdate
  where
    proceedUpdate u = do
      pushUpdate u
      offsetOfUpdate u >>= updateOffset


translator :: (MonadTranslator m,
               HasUpdateQueue m,
               HasMessageQueue m,
               RepetitionsHandler m) => m ()
translator = pullUpdate
  >>= updateToMessage
  >>= handleRepetitions
  >>= pushMessage


sender :: (MonadSender m, HasMessageQueue m, HasRepetitions m) => m ()
sender = do
  m <- pullMessage
  rs <- chatIdOfMessage m >>= getRepetitions
  replicateM_ rs (sendMessage m)
