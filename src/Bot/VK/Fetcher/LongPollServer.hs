{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Fetcher.LongPollServer where

import           Data.Aeson         (FromJSON (parseJSON), withObject, (.:))
import           Data.Aeson.Types   (Parser)
import           Data.List          (foldl')
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Network.HTTP.Req   (Scheme (Https), Url, https, (/:))
import           Text.Read          (readMaybe)


data LongPollServer = LongPollServer
  { key        :: Text
  , serverAddr :: Url 'Https
  , ts         :: Integer
  } deriving (Eq, Show)


instance FromJSON LongPollServer where
  parseJSON = withObject "LongPollServer" $ \obj -> do
    s <- obj .: "response"

    let readField :: (FromJSON a, Read a) => Text -> Parser a
        readField f = (s .: f)
          >>= maybe (fail $ "Unreadable field: " <> Text.unpack f) pure
          . readMaybe

    LongPollServer
      <$> s .: "key"
      <*> (s .: "server" >>= textToUrl)
      <*> readField "ts"


textToUrl :: Text -> Parser (Url 'Https)
textToUrl t
  = case Text.stripPrefix "https://" t of
      Just x ->
        getDomain x >>= addPath x . https
      _ ->
        callFail
  where
    getDomain u = case Text.takeWhile (/='/') u of
      "" -> callFail
      d  -> pure d
    addPath u b = foldl' (/:) b . NE.tail
      <$> isEmpty (Text.splitOn "/" $ Text.dropWhile (/='/') u)
    isEmpty []     = callFail
    isEmpty (x:xs) = pure (x :| xs)
    callFail = fail $ "Url is malformed: " <> Text.unpack t
