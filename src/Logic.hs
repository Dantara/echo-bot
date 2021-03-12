{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Logic where

import           Bot           (HasMessageQueue (pullMessage, pushMessage),
                                HasOffset (updateOffset),
                                HasUpdateQueue (pullUpdate, pushUpdate),
                                MonadFetcher (..), MonadSender (sendMessage),
                                MonadSleep (..), MonadTranslator (..),
                                RepetitionsHandler (..),
                                TechMessage (isTechMessage))
import           Control.Monad ((>=>))


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
               TechMessage m,
               MonadSleep m) => m ()
translator = pullUpdate >>= maybe
  sleep
  (updateToMessage
   >=> handleRepeatCommand
   >=> handleRepeat)
  where
    handleRepeat m = isTechMessage m
      >>= \case
        True ->
          pushMessage m
        False ->
          repeatMessage m >>= mapM_ pushMessage


sender :: (MonadSender m, HasMessageQueue m, MonadSleep m) => m ()
sender = pullMessage
  >>= maybe sleep sendMessage
