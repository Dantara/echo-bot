{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ConfigSpec where

import           Bot
import           Bot.Telegram.Types.Config  (TelegramConfig (..))
import qualified Bot.Telegram.Types.Config  as TG
import           Bot.VK.Types.Config        (VKConfig (..))
import qualified Bot.VK.Types.Config        as VK
import           Config                     (Config (..))
import           ConfigSpec.SampleBotConfig
import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy.Char8 as BLC
import           Logger
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit


test_config :: TestTree
test_config =
  testGroup
    "Config tests"
    [ testCase "Sample config parse" $
        decode (BLC.pack $(getSampleConfig)) @?= Just parsedConfig
    ]
  where
    parsedConfig = Config
      { telegramConfigs = [
          TelegramConfig
            { TG.token = Token
              { extractToken = "1470862909:AAGZF-lhbKci7azP-NHsxiTCdGJ4flHlTDo" }
            , TG.helpMsg = "Arbitrary help message"
            , TG.defaultReps = 1
            , TG.repsQuestion = "What amount of repetitions do you prefer?"
            , TG.logLevel = Info
            , TG.fetcherDelay = 1000000
            , TG.translatorDelay = 1000000
            , TG.senderDelay = 1000000
            , TG.fetchersAmount = 1
            , TG.translatorsAmount = 1
            , TG.sendersAmount = 1
            }]
      , vkConfigs = [
          VKConfig
            { VK.token = Token
                { extractToken = "3e142ca1109e4dbac9008679226910e66eed\
                                 \123f50367fd8b679d38854a88ad888e57b3479331fb2b46b9"
                }
            , VK.groupId = "201676497"
            , VK.fetcherTimeout = 25
            , VK.helpMsg = "Arbitrary help message"
            , VK.defaultReps = 1
            , VK.repsQuestion = "What amount of repetitions do you prefer?"
            , VK.logLevel = Info
            , VK.fetcherDelay = 1000000
            , VK.translatorDelay = 1000000
            , VK.senderDelay = 1000000
            , VK.fetchersAmount = 1
            , VK.translatorsAmount = 1
            , VK.sendersAmount = 1
            }]
      }
