{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bot.Telegram.Types.UpdatesSpec where

import           Bot.Telegram.Types.Shared
import           Bot.Telegram.Types.Updates
import           Bot.Telegram.Types.UpdatesSpec.SampleUpdates
import           Data.Aeson                                   (decode)
import           Data.ByteString.Lazy.Char8                   as BLC
import           Network.HTTP.Req
import           Test.Tasty                                   (TestTree,
                                                               testGroup)
import           Test.Tasty.HUnit

test_vk_updates :: TestTree
test_vk_updates =
  testGroup
    "Telegram Updates tests"
    [ testCase "Sample Telegram updates parse" $
        decode (BLC.pack $(getSampleUpdates)) @?= Just parsedUpdates
    ]
  where
    parsedUpdates =
      Updates
        { extractUpdates =
            [ Upd
                { updateId = 531360739,
                  message =
                    ReceivedMsg
                      { receivedChatId = 68941102,
                        receivedMessageId = 230,
                        receivedText = Nothing,
                        receivedAudio = Nothing,
                        receivedDocument = Nothing,
                        receivedPhoto =
                          Just
                            [ FileInfo
                                { fileId = "AgACAgIAAxkBAAPmYC8lJFb\
                                           \YushmtpkKfqkgRGoEupYAAk\
                                           \a1MRuAWXlJaFiwf-5SKrjgO\
                                           \gmeLgADAQADAgADbQADhAQAAh4E"
                                },
                              FileInfo
                                { fileId = "AgACAgIAAxkBAAPmYC8lJFb\
                                           \YushmtpkKfqkgRGoEupYAAk\
                                           \a1MRuAWXlJaFiwf-5SKrjgO\
                                           \gmeLgADAQADAgADeAADhQQAAh4E"
                                },
                              FileInfo
                                { fileId = "AgACAgIAAxkBAAPmYC8lJFb\
                                           \YushmtpkKfqkgRGoEupYAAk\
                                           \a1MRuAWXlJaFiwf-5SKrjgO\
                                           \gmeLgADAQADAgADeQADggQAAh4E"
                                }
                            ],
                        receivedSticker = Nothing,
                        receivedVideo = Nothing,
                        receivedVideoNote = Nothing,
                        receivedVoice = Nothing,
                        receivedCaption = Just "hello",
                        receivedContact = Nothing,
                        receivedDice = Nothing,
                        receivedVenue = Nothing,
                        receivedMessageLocation = Nothing
                      }
                }
            ]
        }
