{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types.Msg where

import           Bot.VK.Types.Shared
import           Data.Aeson
import           Data.Text           (Text)
import qualified Data.Text           as Text


data Msg = Msg
  { userId      :: Integer
  , randomId    :: Int
  , text        :: Text
  , attachments :: [Attachment]
  }


serializeAttachments :: [Attachment] -> Text
serializeAttachments = Text.pack . foldMap ((<> ",") <$> serAttachment)
  where
    serAttachment (Photo fi)        = "photo" <> serFileInfo fi
    serAttachment (Document fi)     = "doc" <> serFileInfo fi
    serAttachment UnknownAttachment = ""
    serFileInfo (FileInfo mid oid) = show oid <> "_" <> show mid
