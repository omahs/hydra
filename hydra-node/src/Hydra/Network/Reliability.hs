{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A `Network` layer that guarantees delivery of `msg` in order even in the
-- face of transient connection failures.
--
-- This layer implements an algorithm based on _vector clocks_, loosely inspired from
-- /Reliable consistent broadcast/ algorithms with FIFO ordering as presented in
-- [Introduction to Reliable and Secure Distributed
-- Programming](https://www.distributedprogramming.net), ch. 3.9, by Cachin et al.
--
-- Each node maintains a vector of monotonically increasing integer
-- indices denoting the index of the last message known (sent or received) for
-- each peer, where a peer is identified a `Party`, which is updated upon
-- sending and receiving messages, and is sent with each message.
--
-- The basic algorithm is simple:
--
--   * When a message is sent, the index of the current node's party is incremented,
--
--   * When a message is received:
--
--       * It is discarded if the index for the sender's party in the message is
--         not exactly one more than the latest known index,
--
--       * If our own party's index as broadcasted by the sender is lower than our
--         latest known index, we resend all the "missing" messages.
--
-- As shown by the signature of the `withReliability` function, this layer
-- relies on an authentication layer providing `Authenticated` messages in order
-- to securely identify senders, and also on `Heartbeat` messages in order to
-- provide some "liveness".
--
-- `Heartbeat` messages are critical in order to /signal/ peers our current view
-- of the world, because it could be the case that we don't have any network
-- message to send which leads to head being stalled. `Ping` messages are
-- treated specially, both when receiving and sending:
--
--   * When sending a `Ping`, we /don't increment/ our local message counter
--     before broadcasting it,
--
--   * Conversely, when receiving a `Ping`, we don't update the peer's message
--     counter but only take into account their view of /our/ counter in order
--     to compute whether or not to resend previous messages.
--
-- NOTE: This layer does not guarantee resilience in the crash-recovery setting,
-- eg. if a process crashes and then later recovers. It may work because `Ping`s
-- will trigger some resending to the peer which starts from scratch. To provide
-- this guarantee in full, we should add /logging/ capability that persist sent and
-- received messages before communicating with the applicative layer.
module Hydra.Network.Reliability where

import Hydra.Prelude hiding (empty, fromList, length, replicate, zipWith)

import Cardano.Binary (serialize')
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTQueue, readTVarIO, writeTQueue),
  modifyTVar',
  newTQueueIO,
  newTVarIO,
  writeTVar,
 )
import Control.Tracer (Tracer)
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import Data.Vector (
  Vector,
  elemIndex,
  fromList,
  generate,
  length,
  replicate,
  zipWith,
  (!?),
 )
import Hydra.Logging (traceWith)
import Hydra.Network (Network (..), NetworkComponent)
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Heartbeat (Heartbeat (..), isPing)
import Hydra.Party (Party)
import Test.QuickCheck (getPositive, listOf)

data ReliableMsg msg = ReliableMsg
  { knownMessageIds :: Vector Int
  -- ^ Vector of highest known counter for each known party. Serves as announcement of
  -- which messages the sender of `ReliableMsg` has seen. The individual counters have
  -- nothing to do with the `message` also included in this.
  , message :: msg
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (ToCBOR msg) => ToCBOR (ReliableMsg msg) where
  toCBOR ReliableMsg{knownMessageIds, message} = toCBOR knownMessageIds <> toCBOR message

instance (FromCBOR msg) => FromCBOR (ReliableMsg msg) where
  fromCBOR = ReliableMsg <$> fromCBOR <*> fromCBOR

instance ToCBOR msg => SignableRepresentation (ReliableMsg msg) where
  getSignableRepresentation = serialize'

data ReliabilityLog
  = Resending {missing :: Vector Int, acknowledged :: Vector Int, localCounter :: Vector Int, partyIndex :: Int}
  | BroadcastCounter {partyIndex :: Int, localCounter :: Vector Int}
  | BroadcastPing {partyIndex :: Int, localCounter :: Vector Int}
  | Received {acknowledged :: Vector Int, localCounter :: Vector Int, partyIndex :: Int}
  | Ignored {acknowledged :: Vector Int, localCounter :: Vector Int, partyIndex :: Int}
  | ClearedMessageQueue {messageQueueLength :: Int, deletedMessage :: Int}
  | ReliabilityFailedToFindMsg {failedToFindMessage :: String}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary (Vector Int) where
  arbitrary = fromList <$> listOf (getPositive <$> arbitrary)

instance Arbitrary ReliabilityLog where
  arbitrary = genericArbitrary

data SentMessages m msg = SentMessages
  { getSentMessages :: m (IMap.IntMap msg)
  , insertSentMessage :: Int -> msg -> m ()
  , removeSentMessage :: Int -> m (IMap.IntMap msg)
  }

data SeenMessages m = SeenMessages
  { getSeenMessages :: m (Map.Map Party Int)
  , insertSeenMessage :: Party -> Int -> m ()
  }

data AckCounter m = AckCounter
  { getAckCounter :: m (Vector Int)
  , updateAckCounter :: Vector Int -> m (Vector Int)
  }

-- | Middleware function to handle message counters tracking and resending logic.
--
-- '''NOTE''': There is some "abstraction leak" here, because the `withReliability`
-- layer is tied to a specific structure of other layers, eg. be between
-- `withHeartbeat` and `withAuthenticate` layers.
--
-- NOTE: better use of Vectors? We should perhaps use a `MVector` to be able to
-- mutate in-place and not need `zipWith`
withReliability ::
  (MonadThrow m, MonadAsync m) =>
  Tracer m ReliabilityLog ->
  -- | Our own party identifier.
  Party ->
  -- | Other parties' identifiers.
  [Party] ->
  -- | Underlying network component providing consuming and sending channels.
  NetworkComponent m (Authenticated (ReliableMsg (Heartbeat msg))) (ReliableMsg (Heartbeat msg)) a ->
  NetworkComponent m (Authenticated (Heartbeat msg)) (Heartbeat msg) a
withReliability tracer me otherParties withRawNetwork callback action = do
  ackCounter <- mkAckCounterHandle allParties
  sentMessages <- mkSentMessagesHandle
  seenMessages <- mkSeenMessagesHandle otherParties
  resendQ <- newTQueueIO
  let mMyIndex = elemIndex me allParties
  let resend = writeTQueue resendQ
  withRawNetwork (reliableCallback ackCounter sentMessages seenMessages resend) $ \network@Network{broadcast} -> do
    withAsync (forever $ atomically (readTQueue resendQ) >>= broadcast) $ \_ ->
      reliableBroadcast mMyIndex ackCounter sentMessages network
 where
  allParties = fromList $ sort $ me : otherParties

  reliableBroadcast Nothing _ _ _ = action Network{broadcast = \_ -> pure ()}
  reliableBroadcast (Just myIndex) AckCounter{getAckCounter, updateAckCounter} SentMessages{insertSentMessage} Network{broadcast} =
    action $
      Network
        { broadcast = \msg ->
            case msg of
              Data{} -> do
                ackCounter' <- do
                  acks <- getAckCounter
                  let newAcks = incAckForParty acks myIndex
                  forM_ (newAcks !? myIndex) $ \myAcks ->
                    insertSentMessage myAcks msg
                  updateAckCounter newAcks

                traceWith tracer (BroadcastCounter myIndex ackCounter')
                broadcast $ ReliableMsg ackCounter' msg
              Ping{} -> do
                acks <- getAckCounter
                traceWith tracer (BroadcastPing myIndex acks)
                broadcast $ ReliableMsg acks msg
        }

  reliableCallback AckCounter{getAckCounter, updateAckCounter} sentMessages seenMessages resend (Authenticated (ReliableMsg acks msg) party) = do
    if length acks /= length allParties
      then ignoreMalformedMessages
      else findPartyIndex party $ \partyIndex -> do
        knownAcks' <- getAckCounter

        forM_ (findAcksFor knownAcks' acks partyIndex) $ \(messageAckForParty, knownAckForParty) -> do
          (shouldCallback, knownAcks) <- do
            -- handle message from party iff it's next in line OR if it's a Ping
            --
            -- The stream of messages from a peer is expected to look like:
            --
            -- @@
            -- Msg [.., 1, ..] (Data nid m1)
            -- Msg [.., 2, ..] (Data nid m1)
            -- Msg [.., 3, ..] (Data nid m1)
            -- Msg [.., 3, ..] (Ping nid)
            -- Msg [.., 3, ..] (Ping nid)
            -- Msg [.., 3, ..] (Ping nid)
            -- @@
            --
            -- Pings are observed only for the information it provides about the
            -- peer's view of our index
            if
                | isPing msg ->
                    return (isPing msg, knownAcks')
                | messageAckForParty == knownAckForParty + 1 -> do
                    let newAcks = incAckForParty knownAcks' partyIndex
                    void $ updateAckCounter newAcks
                    return (True, newAcks)
                | otherwise ->
                    return (isPing msg, knownAcks')

          if shouldCallback
            then do
              callback (Authenticated msg party)
              traceWith tracer (Received acks knownAcks partyIndex)
            else traceWith tracer (Ignored acks knownAcks partyIndex)

          when (isPing msg) $
            resendMessagesIfLagging resend partyIndex sentMessages knownAcks acks

          -- Update last message index sent by us and seen by some party
          updateSeenMessages seenMessages acks party
          -- Take the lowest number from seen messages by everyone and remove it from
          -- our sent messages.
          deleteSeenMessage sentMessages seenMessages

  ignoreMalformedMessages = pure ()

  incAckForParty acks wantedIndex =
    zipWith (\ack i -> if i == wantedIndex then ack + 1 else ack) acks partyIndexes

  partyIndexes = generate (length allParties) id

  resendMessagesIfLagging resend partyIndex SentMessages{getSentMessages} knownAcks messageAcks =
    findPartyIndex me $ \myIndex -> do
      forM_ (findAcksFor knownAcks messageAcks myIndex) $ \(messageAckForUs, knownAckForUs) -> do
        -- We resend messages if our peer notified us that it's lagging behind our
        -- latest message sent
        when (messageAckForUs < knownAckForUs) $ do
          let missing = fromList [messageAckForUs + 1 .. knownAckForUs]
          messages <- getSentMessages
          forM_ missing $ \idx -> do
            case messages IMap.!? idx of
              -- Here we decide to just log and continue even if we are not able to
              -- find the message to resend. The reason is we don't want to bring
              -- the network down by throwing just because some message is
              -- missplaced.
              Nothing -> do
                traceWith tracer $
                  ReliabilityFailedToFindMsg
                    ( show idx
                        <> ", messages length = "
                        <> show (IMap.size messages)
                        <> ", latest message ack: "
                        <> show knownAckForUs
                        <> ", acked: "
                        <> show messageAckForUs
                    )
                pure ()
              Just missingMsg -> do
                let newAcks' = zipWith (\ack i -> if i == myIndex then idx else ack) knownAcks partyIndexes
                traceWith tracer (Resending missing messageAcks newAcks' partyIndex)
                atomically $ resend $ ReliableMsg newAcks' missingMsg

  updateSeenMessages SeenMessages{insertSeenMessage} acks party = do
    findPartyIndex me $ \myIndex -> do
      forM_ (acks !? myIndex) $ \messageAckForUs ->
        insertSeenMessage party messageAckForUs

  -- We delete messages by obtaining the minimim counter from a map of seen
  -- messages by parties. This implies that all other parties must already have
  -- seen the same message since their counter is higher than the minimum one.
  --
  -- Eg. [(P1, 3), (P2, 4) (P3,1)] -> Here we would pick P3 that saw the message
  -- 1 to delete since P1 and P2 already saw messages 3 and 4 which come after
  -- the message 1 thus it is safe to delete this message.
  deleteSeenMessage SentMessages{getSentMessages, removeSentMessage} SeenMessages{getSeenMessages} = do
    clearedMessages <- do
      seenMessages' <- getSeenMessages
      let messageReceivedByEveryone = minIndex seenMessages'
      sentMessages' <- getSentMessages
      if IMap.member messageReceivedByEveryone sentMessages'
        then do
          updatedMap <- removeSentMessage messageReceivedByEveryone
          pure $ Just ClearedMessageQueue{messageQueueLength = IMap.size updatedMap, deletedMessage = messageReceivedByEveryone}
        else pure Nothing

    forM_ clearedMessages (traceWith tracer)
   where
    minIndex messages =
      case sortBy (\(_, a) (_, b) -> compare a b) (Map.toList messages) of
        [] -> 0 -- should not happen
        ((_, v) : _) -> v

  -- Find the index of a party in the list of parties.
  -- NOTE: This should never fail so we can ignore errors.
  findPartyIndex party =
    forM_ (elemIndex party allParties)

  -- find the known (local) acks and acks from a message for certain party
  findAcksFor knownAcks acks partyIndex = do
    messageAckForParty <- acks !? partyIndex
    knownAckForParty <- knownAcks !? partyIndex
    pure (messageAckForParty, knownAckForParty)

mkSentMessagesHandle :: MonadSTM m => m (SentMessages m msg)
mkSentMessagesHandle = do
  sentMessages <- newTVarIO IMap.empty
  pure
    SentMessages
      { getSentMessages = readTVarIO sentMessages
      , insertSentMessage = \i msg -> atomically $ modifyTVar' sentMessages (IMap.insert i msg)
      , removeSentMessage = \i -> atomically $ do
          sentMessages' <- readTVar sentMessages
          let updatedMap = IMap.delete i sentMessages'
          writeTVar sentMessages updatedMap
          readTVar sentMessages
      }

mkSeenMessagesHandle :: MonadSTM m => [Party] -> m (SeenMessages m)
mkSeenMessagesHandle parties = do
  seenMessages <- newTVarIO $ Map.fromList $ (,0) <$> toList parties
  pure
    SeenMessages
      { getSeenMessages = readTVarIO seenMessages
      , insertSeenMessage = \i msg -> atomically $ modifyTVar' seenMessages (Map.insert i msg)
      }

mkAckCounterHandle :: MonadSTM m => Vector Party -> m (AckCounter m)
mkAckCounterHandle parties = do
  ackCounter <- newTVarIO $ replicate (length parties) 0
  pure
    AckCounter
      { getAckCounter = readTVarIO ackCounter
      , updateAckCounter = \newAcks -> atomically $ do
          writeTVar ackCounter newAcks
          readTVar ackCounter
      }
