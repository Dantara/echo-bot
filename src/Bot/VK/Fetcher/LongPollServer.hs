{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Fetcher.LongPollServer where

import           Data.Aeson
import           Data.Text  (Text)


data LongPollServer = LongPollServer
  { key        :: Text
  , serverAddr :: Text
  , ts         :: Integer
  }


instance FromJSON LongPollServer where
  parseJSON = withObject "LongPollServer" $ \obj -> do
    s <- obj .: "response"
    LongPollServer
      <$> s .: "key"
      <*> s .: "server"
      <*> s .: "ts"
