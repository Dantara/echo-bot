{-# LANGUAGE OverloadedStrings #-}
module Message where

import           Data.Aeson
import           Data.Text  (Text)


data Message = Message
  { chatId :: Integer
  , text   :: Maybe Text
  } deriving Show


instance FromJSON Message where
  parseJSON = withObject "Message" $ \msg -> Message
    <$> (msg .: "chat" >>= (.: "id"))
    <*> msg .:? "text"
