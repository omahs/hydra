-- | Module to deal with time in direct cardano chain layer. Defines the type
-- for a 'PointInTime' and a means to acquire one via a 'TimeHandle' and
-- 'queryTimeHandle'.
module Hydra.Chain.Direct.TimeHandle where

import Hydra.Prelude

import Cardano.Slotting.Slot (EpochSize (..), SlotNo (SlotNo))
import Cardano.Slotting.Time (
  RelativeTime (..),
  SystemStart (SystemStart),
  fromRelativeTime,
  mkSlotLength,
  toRelativeTime,
 )
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  CardanoMode,
  ChainPoint (..),
  ConsensusMode (CardanoMode),
  EpochNo (..),
  EraHistory (EraHistory),
  NetworkId,
  StandardCrypto,
 )
import Hydra.Chain.CardanoClient (
  QueryPoint (QueryTip),
  queryEraHistory,
  querySystemStart,
  queryTip,
 )
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History (
  Bound (..),
  EraEnd (..),
  EraParams (..),
  EraSummary (..),
  SafeZone (UnsafeIndefiniteSafeZone),
  Summary (Summary),
  initBound,
  mkInterpreter,
 )
import Ouroboros.Consensus.HardFork.History.Qry (
  interpretQuery,
  slotToWallclock,
  wallclockToSlot,
 )
import Ouroboros.Consensus.Util.Counting (NonEmpty (NonEmptyOne))
import Test.QuickCheck (getPositive)

type PointInTime = (SlotNo, UTCTime)

data TimeHandle = TimeHandle
  { currentPointInTime :: Either Text PointInTime
  -- ^ Get the current 'PointInTime'
  , slotFromUTCTime :: UTCTime -> Either Text SlotNo
  -- ^ Lookup slot number given a 'UTCTime'. This will fail if the time is
  -- outside the "safe zone".
  , slotToUTCTime :: SlotNo -> Either Text UTCTime
  -- ^ Convert a slot number to a 'UTCTime' using the stored epoch info. This
  -- will fail if the slot is outside the "safe zone".
  }

instance Arbitrary TimeHandle where
  arbitrary = do
    TimeHandleParams{systemStart, eraHistory, currentSlot} <- genTimeParams
    pure $ mkTimeHandle currentSlot systemStart eraHistory

data TimeHandleParams = TimeHandleParams
  { systemStart :: SystemStart
  , eraHistory :: EraHistory CardanoMode
  , horizonSlot :: SlotNo
  , currentSlot :: SlotNo
  }

-- | Generate consistent values for 'SystemStart' and 'EraHistory' which has
-- a horizon at the returned SlotNo as well as some UTCTime before that
genTimeParams :: Gen TimeHandleParams
genTimeParams = do
  startSeconds <- getPositive <$> arbitrary
  let startTime = posixSecondsToUTCTime $ secondsToNominalDiffTime startSeconds
  uptimeSeconds <- getPositive <$> arbitrary
  -- it is ok to construct a slot from seconds here since on the devnet slot = 1s
  let currentSlotNo = SlotNo $ truncate $ uptimeSeconds + startSeconds
      -- formula: 3 * k / f where k = securityParam and f = slotLength from the genesis config
      safeZone = 3 * 2160 / 0.05
      horizonSlot = SlotNo $ truncate $ uptimeSeconds + safeZone
  pure $
    TimeHandleParams
      { systemStart = SystemStart startTime
      , eraHistory = eraHistoryWithHorizonAt horizonSlot
      , horizonSlot = horizonSlot
      , currentSlot = currentSlotNo
      }

-- | An era history with a single era which will end at some point.
--
-- A "real" 'EraHistory' received from the cardano-node will have the 'eraEnd'
-- at a known or earliest possible end of the current era + a safe zone.
--
-- See 'Ouroboros.Consensus.HardFork.History.EraParams' for details.
--
-- NOTE: This era is using not so realistic epoch sizes of 1 and sets a slot
-- length of 1
eraHistoryWithHorizonAt :: SlotNo -> EraHistory CardanoMode
eraHistoryWithHorizonAt slotNo@(SlotNo n) =
  EraHistory CardanoMode (mkInterpreter summary)
 where
  summary :: Summary (CardanoEras StandardCrypto)
  summary =
    Summary . NonEmptyOne $
      EraSummary
        { eraStart = initBound
        , eraEnd =
            EraEnd $
              Bound
                { boundTime = RelativeTime $ fromIntegral n
                , boundSlot = slotNo
                , boundEpoch = EpochNo n
                }
        , eraParams
        }

  eraParams =
    EraParams
      { eraEpochSize = EpochSize 1
      , eraSlotLength = mkSlotLength 1
      , -- NOTE: unused if the 'eraEnd' is already defined, but would be used to
        -- extend the last era accordingly in the real cardano-node
        eraSafeZone = UnsafeIndefiniteSafeZone
      }

-- | Construct a time handle using current slot and given chain parameters. See
-- 'queryTimeHandle' to create one by querying a cardano-node.
mkTimeHandle ::
  HasCallStack =>
  SlotNo ->
  SystemStart ->
  EraHistory CardanoMode ->
  TimeHandle
mkTimeHandle currentSlotNo systemStart eraHistory = do
  TimeHandle
    { currentPointInTime = do
        pt <- slotToUTCTime currentSlotNo
        pure (currentSlotNo, pt)
    , slotFromUTCTime
    , slotToUTCTime
    }
 where
  slotToUTCTime :: HasCallStack => SlotNo -> Either Text UTCTime
  slotToUTCTime slot =
    case interpretQuery interpreter (slotToWallclock slot) of
      Left pastHorizonEx -> Left $ show pastHorizonEx
      Right (relativeTime, _slotLength) -> pure $ fromRelativeTime systemStart relativeTime

  slotFromUTCTime :: HasCallStack => UTCTime -> Either Text SlotNo
  slotFromUTCTime utcTime = do
    let relativeTime = toRelativeTime systemStart utcTime
    case interpretQuery interpreter (wallclockToSlot relativeTime) of
      Left pastHorizonEx -> Left $ show pastHorizonEx
      Right (slotNo, _timeSpentInSlot, _timeLeftInSlot) -> pure slotNo

  (EraHistory _ interpreter) = eraHistory

-- | Query node for system start and era history before constructing a
-- 'TimeHandle' using the slot at the tip of the network.
queryTimeHandle :: NetworkId -> FilePath -> IO TimeHandle
queryTimeHandle networkId socketPath = do
  tip <- queryTip networkId socketPath
  systemStart <- querySystemStart networkId socketPath QueryTip
  eraHistory <- queryEraHistory networkId socketPath QueryTip
  currentTipSlot <-
    case tip of
      ChainPointAtGenesis -> pure $ SlotNo 0
      ChainPoint slotNo _ -> pure slotNo

  pure $ mkTimeHandle currentTipSlot systemStart eraHistory
