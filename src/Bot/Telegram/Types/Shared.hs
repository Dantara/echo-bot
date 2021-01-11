{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Types.Shared where

import           Data.Aeson
import           Data.Char    (toLower)
import           Data.Text    (Text)
import           GHC.Generics
import           Helpers      (camelToSnakeCase)


newtype FileInfo = FileInfo { fileId :: Text }


data Contact = Contact
  { phoneNumber :: Text
  , firstName   :: Text
  , lastName    :: Maybe Text
  , vcard       :: Maybe Text
  } deriving (Generic)


newtype Dice = Dice { diceEmoji :: Text }
  deriving (Generic)


data Location = Location
  { longitude :: Float
  , latitude  :: Float
  } deriving (Generic, FromJSON)


data Venue = Venue
  { venueLocation :: Location
  , venueTitle    :: Text
  , venueAddress  :: Text
  } deriving (Generic)


-- * Parsing section


instance FromJSON Contact where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelToSnakeCase }


instance FromJSON Dice where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 4 }


instance FromJSON Venue where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 5 }


instance FromJSON FileInfo where
  parseJSON = withObject "FileInfo" $ \f -> FileInfo
    <$> f .: "file_id"
