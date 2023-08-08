{-# LANGUAGE TypeApplications #-}

-- | Simplified interface to evaluation (phase-2 validation) of individual
-- plutus scripts and transactions.
--
-- The `evaluateTx` function simplifies the call to ledger and plutus providing
-- an 'EvaluationReport' using fixed `Hydra.Fixtures.ProtocolParameters`. This
-- should only be used for /testing/ or /benchmarking/ purpose as the real
-- evaluation parameters should be configured or queried from the network.
module Hydra.Fixtures.Evaluate where

import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Ledger
import Cardano.Ledger.Alonzo.Scripts (txscriptfee)
import Cardano.Ledger.Babbage.PParams (_costmdls)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Val (Val ((<+>)), (<×>))
import Control.Arrow (left)
import qualified Data.ByteString as BS
import Flat (flat)
import Hydra.Cardano.Api (
  CardanoEra (BabbageEra),
  Era,
  ExecutionUnits (..),
  IsShelleyBasedEra (shelleyBasedEra),
  LedgerEpochInfo (..),
  Lovelace,
  ProtocolParameters (..),
  ScriptExecutionError (ScriptErrorMissingScript),
  ScriptWitnessIndex,
  SerialiseAsCBOR (serialiseToCBOR),
  TransactionValidityError,
  Tx,
  UTxO,
  bundleProtocolParams,
  evaluateTransactionExecutionUnits,
  fromLedgerCoin,
  getTxBody,
  shelleyBasedEra,
  toAlonzoPrices,
  toLedgerExUnits,
  toLedgerPParams,
  toLedgerTx,
  toLedgerUTxO,
 )
import Hydra.Fixtures.ProtocolParameters (maxTxExecutionUnits)
import qualified Hydra.Fixtures.ProtocolParameters as Fixtures
import qualified Hydra.Fixtures.Time as Fixtures
import qualified PlutusCore as PLC
import PlutusLedgerApi.Common (mkTermToEvaluate)
import qualified PlutusLedgerApi.Common as Plutus
import qualified UntypedPlutusCore as UPLC

-- * Evaluate transactions

-- | Thin wrapper around 'evaluateTransactionExecutionUnits', using fixtures
-- from this module for 'systemStart', 'eraHistory' and 'pparams'.
--
-- Additionally, this function checks the overall execution units are not
-- exceeding 'maxTxExecutionUnits'.
evaluateTx ::
  Tx ->
  UTxO ->
  Either EvaluationError EvaluationReport
evaluateTx = evaluateTx' maxTxExecutionUnits

-- | Like 'evaluateTx', but with a configurable maximum transaction
-- 'ExecutionUnits'.
evaluateTx' ::
  -- | Max tx execution units.
  ExecutionUnits ->
  Tx ->
  UTxO ->
  Either EvaluationError EvaluationReport
evaluateTx' maxUnits tx utxo =
  case result of
    Left txValidityError -> Left $ TransactionInvalid txValidityError
    Right report
      -- Check overall budget when all individual scripts evaluated
      | all isRight report -> checkBudget maxUnits report
      | otherwise -> Right report
 where
  result =
    evaluateTransactionExecutionUnits
      Fixtures.systemStart
      (LedgerEpochInfo Fixtures.epochInfo)
      (bundleProtocolParams BabbageEra Fixtures.pparams{protocolParamMaxTxExUnits = Just maxUnits})
      (UTxO.toApi utxo)
      (getTxBody tx)

-- | Check the budget used by provided 'EvaluationReport' does not exceed given
-- maximum 'ExecutionUnits'.
checkBudget :: ExecutionUnits -> EvaluationReport -> Either EvaluationError EvaluationReport
checkBudget maxUnits report
  | usedMemory <= executionMemory maxUnits && usedCpu <= executionSteps maxUnits =
      Right report
  | otherwise =
      Left
        TransactionBudgetOverspent
          { used
          , available = maxUnits
          }
 where
  used@ExecutionUnits
    { executionMemory = usedMemory
    , executionSteps = usedCpu
    } = usedExecutionUnits report

-- | Errors returned by 'evaluateTx' extending the upstream
-- 'TransactionValidityError' with additional cases.
data EvaluationError
  = TransactionBudgetOverspent {used :: ExecutionUnits, available :: ExecutionUnits}
  | TransactionInvalid TransactionValidityError
  deriving (Show)

-- | Evaluation result for each of the included scripts. Either they failed
-- evaluation or used a number of 'ExecutionUnits'.
type EvaluationReport =
  (Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits))

renderEvaluationReportFailures :: EvaluationReport -> Text
renderEvaluationReportFailures reportMap =
  unlines $ renderScriptExecutionError <$> failures
 where
  failures = lefts $ foldMap (: []) reportMap

  renderScriptExecutionError = \case
    ScriptErrorMissingScript missingRdmrPtr _ ->
      "Missing script of redeemer pointer " <> show missingRdmrPtr
    f ->
      show f

-- | Get the total used 'ExecutionUnits' from an 'EvaluationReport'. Useful to
-- further process the result of 'evaluateTx'.
usedExecutionUnits :: EvaluationReport -> ExecutionUnits
usedExecutionUnits report =
  ExecutionUnits
    { executionMemory = usedMemory
    , executionSteps = usedCpu
    }
 where
  usedMemory = sum $ executionMemory <$> budgets

  usedCpu = sum $ executionSteps <$> budgets

  budgets = rights $ toList report

-- | Estimate minimum fee for given transaction and evaluated redeemers. Instead
-- of using the budgets from the transaction (which are usually set to 0 until
-- balancing), this directly computes the fee from transaction size and the
-- units of the 'EvaluationReport'. Note that this function only provides a
-- rough estimate using this modules' 'pparams' and likely under-estimates cost
-- as we have no witnesses on this 'Tx'.
estimateMinFee ::
  Tx ->
  EvaluationReport ->
  Lovelace
estimateMinFee tx evaluationReport =
  fromLedgerCoin $
    (txSize <×> a <+> b)
      <+> txscriptfee prices allExunits
 where
  txSize = BS.length $ serialiseToCBOR tx
  a = Coin . fromIntegral $ protocolParamTxFeePerByte Fixtures.pparams
  b = Coin . fromIntegral $ protocolParamTxFeeFixed Fixtures.pparams
  prices =
    fromMaybe (error "no prices in protocol param fixture") $
      toAlonzoPrices =<< protocolParamPrices Fixtures.pparams
  allExunits = foldMap toLedgerExUnits . rights $ toList evaluationReport

-- * Profile transactions

-- | Like 'evaluateTx', but instead of actual evaluation, return the
-- flat-encoded, fully applied scripts for each redeemer to be evaluated
-- externally by 'uplc'. Use input format "flat-namedDeBruijn". This can be used
-- to gather profiling information.
--
-- NOTE: This assumes we use 'Babbage' and only 'PlutusV2' scripts are used.
prepareTxScripts ::
  Tx ->
  UTxO ->
  Either String [ByteString]
prepareTxScripts tx utxo = do
  -- Tuples with scripts and their arguments collected from the tx
  results <-
    case Ledger.collectTwoPhaseScriptInputs Fixtures.epochInfo Fixtures.systemStart pp ltx lutxo of
      Left e -> Left $ show e
      Right x -> pure x

  -- Fully applied UPLC programs which we could run using the cekMachine
  programs <- forM results $ \(script, _language, arguments, _exUnits, _costModel) -> do
    let pArgs = Ledger.getPlutusData <$> arguments
    appliedTerm <- left show $ mkTermToEvaluate Plutus.PlutusV2 protocolVersion script pArgs
    pure $ UPLC.Program () (PLC.defaultVersion ()) appliedTerm

  pure $ flat <$> programs
 where
  pp = toLedgerPParams (shelleyBasedEra @Era) Fixtures.pparams

  ltx = toLedgerTx tx

  lutxo = toLedgerUTxO utxo

  protocolVersion =
    let (major, minor) = protocolParamProtocolVersion Fixtures.pparams
     in Plutus.ProtocolVersion
          { Plutus.pvMajor = fromIntegral major
          , Plutus.pvMinor = fromIntegral minor
          }
