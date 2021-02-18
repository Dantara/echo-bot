{-# LANGUAGE OverloadedStrings #-}

module Bot.Shared.RepeatCommandHandler where

import           Bot
import           Data.Text (Text)
import qualified Data.Text as Text
import           Logger
import           Text.Read (readMaybe)


handleMsgWithRepeatCommand ::
  (Logger m, HasRepeatCalls m) =>
  Message m ->
  ChatId ->
  m (Message m)
handleMsgWithRepeatCommand msg ci = do
  addRepCall ci
  logInfo "User wants to update repetitions amount"
  pure msg


handleMsgWithText ::
  (Logger m, HasRepeatCalls m, HasRepetitions m) =>
  Message m ->
  ChatId ->
  Text ->
  (Text -> Message m) ->
  m (Message m)
handleMsgWithText msg ci t msgProd = do
  wasCalled <- wasRepCalled ci

  case (wasCalled, readMaybe $ Text.unpack t) of
    (True, Just i) -> do
      if i >= 1 && i <= 5
        then do
          updateRepetitions i ci
          removeRepCall ci
          logInfo "User repetitions was updated"
          pure $ msgProd "Repetitions amount was updated!"
        else do
          logWarning "User supplied wrong number of repetitions"
          let t' = "Wrong number of repetitions.\n"
                  <> "Number should lie between 1 and 5."
          pure $ msgProd t'
    (True, Nothing) -> do
      logWarning "User supplied malformed number of repetitions"
      pure msg
    (False, _) ->
      pure msg
