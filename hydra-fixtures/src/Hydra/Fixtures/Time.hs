{-# LANGUAGE TypeApplications #-}

-- | Fixtures for time-based cardano values to run tests and benchmarks with.
module Hydra.Fixtures.Time where

import Hydra.Prelude hiding (label)

import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import Cardano.Ledger.Babbage.PParams (_protocolVersion)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (EpochSize), SlotNo (SlotNo))
import Cardano.Slotting.Time (
  RelativeTime (RelativeTime),
  SlotLength (getSlotLength),
  SystemStart (SystemStart),
  mkSlotLength,
  toRelativeTime,
 )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  Era,
  IsShelleyBasedEra (shelleyBasedEra),
  shelleyBasedEra,
  toLedgerPParams,
 )
import Hydra.Fixtures.ProtocolParameters (pparams)
import Hydra.Plutus.Time (posixToUTCTime)
import Test.QuickCheck (choose)

-- | An artifical 'EpochInfo' comprised by a single never ending (forking) era,
-- with fixed 'epochSize' and 'slotLength'.
epochInfo :: Monad m => EpochInfo m
epochInfo = fixedEpochInfo epochSize slotLength

epochSize :: EpochSize
epochSize = EpochSize 100

slotLength :: SlotLength
slotLength = mkSlotLength 1

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

-- | Using hard-coded 'systemStart' and 'slotLength'. Do not use in production!
slotNoFromUTCTime :: UTCTime -> SlotNo
slotNoFromUTCTime utcTime =
  SlotNo $ truncate (relativeTime / getSlotLength slotLength)
 where
  (RelativeTime relativeTime) =
    toRelativeTime systemStart utcTime

-- | Using hard-coded 'pparams', 'epochInfo', and 'systemStart'. Do not use in
-- production! Fails for slots past epoch boundaries.
slotNoToUTCTime :: HasCallStack => SlotNo -> UTCTime
slotNoToUTCTime =
  either error posixToUTCTime
    . slotToPOSIXTime
      (toLedgerPParams (shelleyBasedEra @Era) pparams)
      epochInfo
      systemStart

genPointInTime :: Gen (SlotNo, UTCTime)
genPointInTime = do
  slot <- SlotNo <$> arbitrary
  let time = slotNoToUTCTime slot
  pure (slot, time)

genPointInTimeBefore :: UTCTime -> Gen (SlotNo, UTCTime)
genPointInTimeBefore deadline = do
  let SlotNo slotDeadline = slotNoFromUTCTime deadline
  slot <- SlotNo <$> choose (0, slotDeadline)
  pure (slot, slotNoToUTCTime slot)

genPointInTimeAfter :: UTCTime -> Gen (SlotNo, UTCTime)
genPointInTimeAfter deadline = do
  let SlotNo slotDeadline = slotNoFromUTCTime deadline
  slot <- SlotNo <$> choose (slotDeadline, maxBound)
  pure (slot, slotNoToUTCTime slot)
