{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bot.VK.Fetcher.LongPollServerSpec where

import           Bot.VK.Fetcher.LongPollServer
import           Bot.VK.Fetcher.LongPollServerSpec.SampleLongPollServer
import           Data.Aeson                                             (decode)
import           Data.ByteString.Lazy.Char8                             as BLC
import           Network.HTTP.Req
import           Test.Tasty                                             (TestTree,
                                                                         testGroup)
import           Test.Tasty.HUnit


test_long_poll_server :: TestTree
test_long_poll_server =
  testGroup
    "Long Poll Server tests"
    [ testCase "Sample long poll server parse" $
        decode (BLC.pack $(getSampleLPS)) @?= Just parsedLPS
    ]
  where
    parsedLPS = LongPollServer
      { key = "21hc7ya30a693a774077cef7ee0d4744122c4b7f"
      , serverAddr = https "lp.vk.com" /: "wh201676497"
      , ts = 285
      }
