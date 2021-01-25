{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types.Updates where

import           Bot.VK.Types.Shared
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Text.Read           (readMaybe)


data Upd = Upd
  { updateId        :: Integer
  , receivedMessage :: ReceivedMsg
  } deriving Show


data Updates = Updates
  { updatesId       :: Integer
  , receivedUpdates :: [ReceivedUpd]
  } deriving Show


data ReceivedUpd
  = MessageUpd ReceivedMsg
  | OtherUpd
  deriving (Eq, Show)


data ReceivedMsg = ReceivedMsg
  { fromId              :: Integer
  , receivedText        :: Text
  , receivedAttachments :: [Attachment]
  } deriving (Eq, Show)



instance FromJSON Updates where
  parseJSON = withObject "Updates" $ \us -> do
    let readField :: (FromJSON a, Read a) => Text -> Parser a
        readField f = (us .: f)
          >>= maybe (fail $ "Unreadable field: " <> Text.unpack f) pure
          . readMaybe

    Updates
      <$> readField "ts"
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
