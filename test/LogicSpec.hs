{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module LogicSpec where

import           Bot
import           Control.Monad.State.Lazy
import           Data.Functor             ((<&>))
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)
import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range
import           Helpers
import           Logger
import           Logic
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog


newtype TestM a = TestM { unwrapTestM :: State TestState a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState TestState
           )


data TestState = TestState
  { updatesReceived :: [Upd]
  , updateQueue     :: Queue Upd
  , messageQueue    :: Queue Msg
  , globalOffset    :: Integer
  , sendedMessages  :: [Msg]
  , repetitions     :: Map ChatId Int
  }


data Upd = Upd
  { updOffset  :: Integer
  , updChatId  :: ChatId
  , updContent :: Content
  } deriving (Eq, Show)

data Msg = Msg
  { msgOffset  :: Integer
  , msgChatId  :: ChatId
  , msgContent :: Content
  }

data Content = Content
  deriving (Eq, Show)


instance Logger TestM where
  log = logIdentity
  getLogLevel = pure Debug


instance MonadFetcher TestM where
  fetchUpdates = do
    us <- gets updatesReceived
    modify (\s -> s { updatesReceived = [] })
    pure us

  offsetOfUpdate (Upd x _ _) = pure x


instance HasUpdateQueue TestM where
  type Update TestM = Upd

  pullUpdate = do
    mU <- gets (pullFromQueue . updateQueue)
    case mU of
      Just (u, uq) -> do
        modify (\s -> s { updateQueue = uq })
        pure $ Just u
      Nothing ->
        pure Nothing

  pushUpdate u = do
    us <- gets updateQueue
    modify (\s -> s { updateQueue = pushToQueue u us})


instance HasMessageQueue TestM where
  type Message TestM = Msg

  pullMessage = do
    mM <- gets (pullFromQueue . messageQueue)
    case mM of
      Just (m, mq) -> do
        modify (\s -> s { messageQueue = mq })
        pure $ Just m
      Nothing ->
        pure Nothing

  pushMessage m = do
    ms <- gets messageQueue
    modify (\s -> s { messageQueue = pushToQueue m ms})


instance MonadTranslator TestM where
  updateToMessage (Upd x y z) = pure $ Msg x y z


instance MonadSender TestM where
  sendMessage m = do
    ms <- gets sendedMessages
    modify (\s -> s { sendedMessages = m : ms })

  chatIdOfMessage (Msg _ ci _) = pure ci


instance HasOffset TestM where
  getOffset = gets globalOffset

  updateOffset n = modify (\s -> s { globalOffset = n })


instance HasRepetitions TestM where
  getDefaultRepetitions = pure 1

  getRepetitions ci = do
    rs <- gets repetitions
    defaults <- getDefaultRepetitions
    pure $ fromMaybe defaults (Map.lookup ci rs)

  updateRepetitions n ci = do
    rs <- gets repetitions
    modify (\s -> s { repetitions = Map.insert ci n rs })


instance RepetitionsHandler TestM where
  handleRepeatCommand = pure

  repeatMessage msg
    = (chatIdOfMessage msg >>= getRepetitions)
    <&> (`replicate` msg)


instance MonadSleep TestM where
  sleep = pure ()


test_fetcher :: TestTree
test_fetcher = testGroup "Fetcher tests"
  [ testCase "HelloWorld test case" ("Hello" @?= "Hello")
  , testProperty "UpdateQueue === ReceivedMsgs" updQueueCheck
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
      <*> pure Content
