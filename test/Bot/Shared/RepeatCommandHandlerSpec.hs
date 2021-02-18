{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.Shared.RepeatCommandHandlerSpec where

import           Bot
import           Bot.Shared.RepeatCommandHandler
import           Control.Monad.State.Lazy        (MonadState, State,
                                                  StateT (StateT), gets, modify,
                                                  runState)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Hedgehog                        (Gen, Property, forAll,
                                                  property, (===))
import qualified Hedgehog.Gen                    as Gen
import qualified Hedgehog.Range                  as Range
import           Logger
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.Hedgehog             (testProperty)


newtype TestM a = TestM {unwrapTestM :: State TestState a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState TestState
    )


data TestState = TestState
  { repetitions :: Map ChatId Int,
    repsCalled  :: Set ChatId
  } deriving (Eq, Show)


instance Logger TestM where
  log = logIdentity
  getLogLevel = pure Debug


data Msg = Msg ChatId Text
  deriving (Eq, Show)


instance HasMessageQueue TestM where
  type Message TestM = Msg

  pullMessage = pure Nothing

  pushMessage = const $ pure ()


instance HasRepetitions TestM where
  getDefaultRepetitions = pure 1

  getRepetitions ci = do
    rs <- gets repetitions
    defaults <- getDefaultRepetitions
    pure $ fromMaybe defaults (Map.lookup ci rs)

  updateRepetitions n ci = do
    rs <- gets repetitions
    modify (\s -> s { repetitions = Map.insert ci n rs })


instance HasRepeatCalls TestM where
  addRepCall ci = do
    rs <- gets repsCalled
    modify (\s -> s { repsCalled = Set.insert ci rs })

  removeRepCall ci = do
    rs <- gets repsCalled
    modify (\s -> s { repsCalled = Set.delete ci rs })

  wasRepCalled ci = Set.member ci
    <$> gets repsCalled


test_handle_msg_with_text :: TestTree
test_handle_msg_with_text = testGroup "Translator msg handler"
  [ testProperty "Proper message" properCheck
  , testProperty "Wrong number supplied" wrongNumberCheck
  , testProperty "Malformed message" malformedMsgCheck
  , testProperty "Repeat not called" repeatNotCalledCheck
  ]
  where
    properCheck :: Property
    properCheck = property $ do
      ci <- forAll $ Gen.integral (Range.linear 1 10000)
      msgN <- forAll $ Gen.int (Range.linear 1 5)
      let msgText = Text.pack $ show msgN
      let msg = Msg ci msgText
      let (msg'@(Msg ci' msgText'), s) = runState
                        (unwrapTestM $ handleMsgWithText msg ci msgText (Msg ci))
                        (TestState Map.empty (Set.singleton ci))
      msgText' === "Repetitions amount was updated!"
      repetitions s === Map.singleton ci msgN
      repsCalled s === Set.empty

    wrongNumberCheck :: Property
    wrongNumberCheck = property $ do
      ci <- forAll $ Gen.integral (Range.linear 1 10000)
      msgText <- forAll $ Text.pack . show <$> Gen.int (Range.linear 6 1000)
      let msg = Msg ci msgText
      let (msg'@(Msg ci' msgText'), s) = runState
                        (unwrapTestM $ handleMsgWithText msg ci msgText (Msg ci))
                        (TestState Map.empty (Set.singleton ci))
      let errorMsg = "Wrong number of repetitions.\n"
                     <> "Number should lie between 1 and 5."
      msgText' === errorMsg
      repsCalled s === Set.singleton ci
      repetitions s === Map.empty

    malformedMsgCheck :: Property
    malformedMsgCheck = property $ do
      ci <- forAll $ Gen.integral (Range.linear 1 10000)
      msgText <- forAll $ Gen.text (Range.linear 10 50) Gen.alpha
      let msg = Msg ci msgText
      let s = TestState Map.empty (Set.singleton ci)
      let (msg', s') = runState
                        (unwrapTestM $ handleMsgWithText msg ci msgText (Msg ci))
                        s
      let errorMsg = "Wrong number of repetitions.\n"
                     <> "Number should lie between 1 and 5."
      msg' === msg
      s === s'

    repeatNotCalledCheck :: Property
    repeatNotCalledCheck = property $ do
      ci <- forAll $ Gen.integral (Range.linear 1 10000)
      msgText <- forAll $ Gen.text (Range.linear 10 50) Gen.alpha
      let msg = Msg ci msgText
      let s = TestState Map.empty Set.empty
      let (msg', s') = runState
                        (unwrapTestM $ handleMsgWithText msg ci msgText (Msg ci))
                        s
      let errorMsg = "Wrong number of repetitions.\n"
                     <> "Number should lie between 1 and 5."
      msg' === msg
      s === s'
