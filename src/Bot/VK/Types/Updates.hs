{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types.Updates where

import           Bot.VK.Types.Shared
import           Data.Aeson
import           Data.Text           (Text)


data Upd = Upd
  { updateId        :: Integer
  , receivedMessage :: ReceivedMsg
  }


data Updates = Updates
  { updatesId       :: Integer
  , receivedUpdates :: [ReceivedUpd]
  }


data ReceivedUpd
  = MessageUpd ReceivedMsg
  | OtherUpd
  deriving (Eq)


data ReceivedMsg = ReceivedMsg
  { fromId              :: Integer
  , receivedText        :: Text
  , receivedAttachments :: [Attachment]
  } deriving (Eq)



instance FromJSON Updates where
  parseJSON = withObject "Updates" $ \us -> Updates
    <$> us .: "ts"
    <*> us .: "updates"


instance FromJSON ReceivedUpd where
  parseJSON = withObject "Upd" $ \u -> do
    type' <- u .: "type"
    case type' :: Text of
      "message_new" ->
        MessageUpd <$> (u .: "object" >>= (.: "message"))
      _ ->
        pure OtherUpd


instance FromJSON ReceivedMsg where
  parseJSON = withObject "ReceivedMsg" $ \m -> ReceivedMsg
    <$> m .: "from_id"
    <*> m .: "text"
    <*> m .: "attachments"


updatesToUpds :: Updates -> [Upd]
updatesToUpds us = foldMap mapper (receivedUpdates us)
  where
    mapper (MessageUpd m) = [Upd (updatesId us) m]
    mapper OtherUpd       = []
