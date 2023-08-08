{-# LANGUAGE TypeApplications #-}

-- | Fixed protocol parameters and related values to run tests and benchmarks
-- with. The values used are typically taken from mainnet.
module Hydra.Fixtures.ProtocolParameters where

import Hydra.Prelude hiding (label)

import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2))
import Cardano.Ledger.Alonzo.Scripts (CostModels (CostModels), mkCostModel)
import Data.Default (def)
import qualified Data.Map as Map
import Data.Ratio ((%))
import Hydra.Cardano.Api (
  Era,
  ExecutionUnitPrices (..),
  ExecutionUnits (..),
  IsShelleyBasedEra (shelleyBasedEra),
  ProtocolParameters (..),
  fromAlonzoCostModels,
  fromLedgerPParams,
  shelleyBasedEra,
 )
import PlutusLedgerApi.Test.EvaluationContext (costModelParamsForTesting)

-- | Current (2023-04-12) mainchain protocol parameters.
-- XXX: Avoid specifiying not required parameters here (e.g. max block units
-- should not matter).
-- XXX: Load and use mainnet parameters from a file which we can easily review
-- to be in sync with mainnet.
pparams :: ProtocolParameters
pparams =
  (fromLedgerPParams (shelleyBasedEra @Era) def)
    { protocolParamCostModels =
        fromAlonzoCostModels
          . CostModels
          $ Map.fromList
            [ (PlutusV1, testCostModel PlutusV1)
            , (PlutusV2, testCostModel PlutusV2)
            ]
    , protocolParamMaxTxExUnits = Just maxTxExecutionUnits
    , protocolParamMaxBlockExUnits =
        Just
          ExecutionUnits
            { executionMemory = 62_000_000
            , executionSteps = 40_000_000_000
            }
    , protocolParamProtocolVersion = (7, 0)
    , protocolParamMaxTxSize = maxTxSize
    , protocolParamMaxValueSize = Just 1000000000
    , protocolParamTxFeePerByte = 44 -- a
    , protocolParamTxFeeFixed = 155381 -- b
    , protocolParamPrices =
        Just
          ExecutionUnitPrices
            { priceExecutionSteps = 721 % 10000000
            , priceExecutionMemory = 577 % 10000
            }
    }
 where
  testCostModel pv =
    case mkCostModel pv costModelParamsForTesting of
      Left e -> error $ "testCostModel failed: " <> show e
      Right cm -> cm

-- | Max transaction size of the current 'pparams'.
maxTxSize :: Natural
maxTxSize = 16384

-- | Max transaction execution unit budget of the current 'pparams'.
maxTxExecutionUnits :: ExecutionUnits
maxTxExecutionUnits =
  ExecutionUnits
    { executionMemory = 14_000_000
    , executionSteps = 10_000_000_000
    }

-- | Max memory and cpu units of the current 'pparams'.
maxMem, maxCpu :: Natural
maxCpu = executionSteps maxTxExecutionUnits
maxMem = executionMemory maxTxExecutionUnits
