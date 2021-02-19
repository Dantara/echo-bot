{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bot.VK.Types.UpdatesSpec where

import           Bot.VK.Types.Shared
import           Bot.VK.Types.Updates
import           Bot.VK.Types.UpdatesSpec.SampleUpdates
import           Data.Aeson                             (decode)
import           Data.ByteString.Lazy.Char8             as BLC
import           Network.HTTP.Req
import           Test.Tasty                             (TestTree, testGroup)
import           Test.Tasty.HUnit

test_vk_updates :: TestTree
test_vk_updates =
  testGroup
    "VK Updates tests"
    [ testCase "Sample VK updates parse" $
        decode (BLC.pack $(getSampleUpdates)) @?= Just parsedUpdates
    ]
  where
    parsedUpdates =
      Updates
        { updatesId = 325,
          receivedUpdates =
            [ OtherUpd,
              MessageUpd
                ( ReceivedMsg
                    { fromId = 58780251,
                      receivedText = "Hello",
                      receivedAttachments =
                        [ Photo
                            ( FileInfo
                                { mediaId = 457239569,
                                  ownerId = 58780251,
                                  accessKey = Nothing
                                }
                            )
                        ]
                    }
                )
            ]
        }
