{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types.Msg where

import           Bot                 (Command)
import           Bot.VK.Types.Shared (Attachment (..), FileInfo (FileInfo))
import           Data.Text           (Text)
import qualified Data.Text           as Text


data Msg = Msg
  { userId      :: Integer
  , randomId    :: Int
  , text        :: Text
  , attachments :: [Attachment]
  , command     :: Maybe Command
  } deriving (Eq)


serializeAttachments :: [Attachment] -> Text
serializeAttachments = Text.pack . foldMap ((<> ",") <$> serAttachment)
  where
    serAttachment (Photo fi)        = "photo" <> serFileInfo fi
    serAttachment (Video fi)        = "video" <> serFileInfo fi
    serAttachment (Audio fi)        = "audio" <> serFileInfo fi
    serAttachment (AudioMsg fi)     = "doc" <> serFileInfo fi
    serAttachment (Document fi)     = "doc" <> serFileInfo fi
    serAttachment (Wall fi)         = "wall" <> serFileInfo fi
    serAttachment (Market fi)       = "market" <> serFileInfo fi
    serAttachment (Poll fi)         = "poll" <> serFileInfo fi
    serAttachment (Sticker _)       = ""
    serAttachment UnknownAttachment = ""
    serFileInfo (FileInfo mid oid mak)
      = show oid
      <> "_"
      <> show mid
      <> maybe "" (("_" <>) <$> Text.unpack) mak
