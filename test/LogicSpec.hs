{-# LANGUAGE TypeFamilies #-}

module LogicSpec where

import           Bot                      (ChatId)
import           Control.Monad.State.Lazy (execState, replicateM_)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import           Hedgehog                 (Gen, Property, forAll, property,
                                           (===))
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range
import           Helpers                  (emptyQueue, listToQueue, queueSize,
                                           queueToList)
import           Logic                    (fetcher, sender, translator)
import           LogicSpec.Internals      (Msg (..), TestM (..), TestState (..),
                                           Upd (..))
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.Hedgehog      (testProperty)


test_fetcher :: TestTree
test_fetcher = testGroup "Fetcher tests"
  [ testProperty "UpdateQueue === ReceivedMsgs" updQueueCheck
  , testProperty "GlobalOffset === Last Upd Offset" offsetCheck
  ]
  where
    updQueueCheck :: Property
    updQueueCheck = property $ do
      upds <- forAll $ Gen.list (Range.linear 0 30) genUpd
      let s = execState
                (unwrapTestM fetcher)
                (TestState upds emptyQueue emptyQueue 0 [] Map.empty)
      queueToList (updateQueue s) === upds

    offsetCheck :: Property
    offsetCheck = property $ do
      upds <- forAll $ Gen.list (Range.linear 1 30) genUpd
      let s = execState
            (unwrapTestM fetcher)
            (TestState upds emptyQueue emptyQueue 0 [] Map.empty)
      globalOffset s === updOffset (last upds)

    genUpd = Upd
      <$> Gen.integral (Range.linear 1 10000)
      <*> Gen.integral (Range.linear 1 10000)
      <*> Gen.text (Range.linear 10 20) Gen.alpha


test_translator :: TestTree
test_translator = testGroup "Translator tests"
  [ testProperty "Correct repetitions amount" repeatCheck
  ]
  where
    repeatCheck :: Property
    repeatCheck = property $ do
      (reps, upds) <- unzip
        <$> forAll (Gen.list (Range.linear 1 30) genRepAndUpd)
      let n = length upds
      let s = execState
            (unwrapTestM $ replicateM_ n translator)
            (TestState [] (listToQueue upds) emptyQueue 0 [] (Map.fromList reps))
      queueSize (messageQueue s) === totalMsgsAmount (repetitions s) upds


test_sender :: TestTree
test_sender = testGroup "Sender tests"
  [ testProperty "Messages Queue === Mesages Send" msgsNotMissing
  ]
  where
    msgsNotMissing :: Property
    msgsNotMissing = property $ do
      msgs <- forAll $ Gen.list (Range.linear 1 30) genMsg
      let n = length msgs
      let s = execState
            (unwrapTestM $ replicateM_ n sender)
            (TestState [] emptyQueue (listToQueue msgs) 0 [] Map.empty)
      msgs === sendedMessages s

    genMsg = Msg
      <$> Gen.integral (Range.linear 1 10000)
      <*> Gen.text (Range.linear 10 20) Gen.alpha


test_all_logic :: TestTree
test_all_logic = testGroup "Full bussiness logic tests"
  [ testProperty "Correct repetitions amount" repeatCheck
  ]
  where
    repeatCheck :: Property
    repeatCheck = property $ do
      (reps, upds) <- unzip
        <$> forAll (Gen.list (Range.linear 1 50) genRepAndUpd)
      let n = length upds
      let totalN = totalMsgsAmount (Map.fromList reps) upds
      let s = execState
            (unwrapTestM $ do
                fetcher
                replicateM_ n translator
                replicateM_ totalN sender)
            (TestState upds emptyQueue emptyQueue 0 [] (Map.fromList reps))
      length (sendedMessages s) === totalN


genRepAndUpd :: Gen ((ChatId, Int), Upd)
genRepAndUpd = do
  o <- Gen.integral (Range.linear 1 10000)
  r <- Gen.integral (Range.linear 1 5)
  ci <- Gen.integral (Range.linear 1 10000)
  t <- Gen.text (Range.linear 10 20) Gen.alpha
  pure ((ci, r), Upd o ci t)


totalMsgsAmount :: Map ChatId Int -> [Upd] -> Int
totalMsgsAmount rs = length
  . foldMap (\x -> replicate (reps x) x)
  where
    reps (Upd _ ci _) = Map.findWithDefault 1 ci rs
