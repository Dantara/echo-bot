module Main where

import qualified Bot.Telegram       as Telegram
import qualified Bot.VK             as VK
import           Config             (Config (..), readConfig)
import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever, void)


main :: IO ()
main = do
  eitherCfg <- readConfig

  case eitherCfg of
    Right cfg -> do
     mapM_ Telegram.runBot (telegramConfigs cfg)
     mapM_ VK.runBot (vkConfigs cfg)
    Left err ->
      putStrLn $ "[Error] Error while parsing config:\n" <> err

  void $ forever $ threadDelay 10000000
