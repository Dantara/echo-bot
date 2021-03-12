{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module LogicSpec.Internals where


import           Bot
import           Control.Monad.State.Lazy (MonadState, State, StateT (StateT),
                                           gets, modify)
import           Data.Functor             ((<&>))
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import           Helpers                  (Queue, pullFromQueue, pushToQueue)
import           Logger                   (LogLevel (..), Logger (..),
                                           logIdentity)


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
  , updContent :: Text
  } deriving (Eq, Show)

data Msg = Msg
  { msgChatId  :: ChatId
  , msgContent :: Text
  } deriving (Eq, Show)

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
  updateToMessage (Upd _ x y) = pure $ Msg x y


instance MonadSender TestM where
  sendMessage m = do
    ms <- gets sendedMessages
    modify (\s -> s { sendedMessages = m : ms })

  chatIdOfMessage (Msg ci _) = pure ci


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


instance TechMessage TestM where
  isTechMessage _ = pure False


instance MonadSleep TestM where
  sleep = pure ()
