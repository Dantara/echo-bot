{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Logic where

import           Bot
import           Control.Monad (replicateM_, (>=>))


fetcher :: (MonadFetcher m, HasUpdateQueue m, HasOffset m, MonadSleep m) => m ()
fetcher = fetchUpdates >>= mapM_ proceedUpdate >> sleep
  where
    proceedUpdate u = do
      pushUpdate u
      offsetOfUpdate u >>= updateOffset


translator :: (MonadTranslator m,
               HasUpdateQueue m,
               HasMessageQueue m,
               RepetitionsHandler m,
               MonadSleep m) => m ()
translator = pullUpdate >>= maybe
  sleep
  (updateToMessage
   >=> handleRepetitions
   >=> pushMessage)


sender :: (MonadSender m, HasMessageQueue m, HasRepetitions m, MonadSleep m) => m ()
sender = do
  msg <- pullMessage
  case msg of
    Just m -> do
      rs <- chatIdOfMessage m >>= getRepetitions
      replicateM_ rs (sendMessage m)
    Nothing ->
      sleep
