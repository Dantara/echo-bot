{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Bot.Telegram.Types.Config (TelegramConfig)
import           Bot.VK.Types.Config       (VKConfig)
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BS


defaultConfigLocation :: String
defaultConfigLocation = "bot.config"


data Config = Config
  { telegramConfigs :: [TelegramConfig]
  , vkConfigs       :: [VKConfig]
  } deriving (Eq, Show)


instance FromJSON Config where
  parseJSON = withObject "Config" $ \c -> Config
    <$> c .: "telegram_configs"
    <*> c .: "vk_configs"


readConfig :: IO (Either String Config)
readConfig = do
  c <- BS.readFile defaultConfigLocation
  pure $ eitherDecode c
