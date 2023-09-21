{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Golden tests of hydra-plutus scripts.
--
-- This test suite ensures we do not accidentally change scripts and also
-- persists the plutus scripts of the Hydra protocol as blobs in the repository.
--
-- This is also crucial in case we cannot reproduce them exactly as they were
-- originally compiled using plutus-tx; which is not unlikely given we need to
-- have the exact same version of plutus-tx, all its dependencies, and GHC.
module Hydra.Plutus.GoldenSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (
  AsType (AsPlutusScriptV2, AsScript),
  File (..),
  Script,
  fromPlutusScript,
  hashScript,
  readFileTextEnvelope,
  writeFileTextEnvelope,
  pattern PlutusScript,
 )
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadTokens as HeadTokens
import qualified Hydra.Contract.Initial as Initial
import Hydra.Plutus (commitValidatorScript)
import Hydra.Version (gitDescribe)
import PlutusLedgerApi.V2 (serialiseCompiledCode)
import qualified PlutusLedgerApi.V2 as Plutus
import System.FilePath ((</>))
import System.Process (
  CreateProcess (..),
  StdStream (UseHandle),
  createProcess,
  proc,
  waitForProcess,
 )
import Test.Hspec.Golden (Golden (..))

spec :: Spec
spec = do
  it "checks plutus blueprint remains the same" $ do
    withTempDir "hydra-plutus-golden" $ \tmpDir -> do
      -- Run 'aiken build' to re-generate plutus.json file
      let aikenLogFilePath = tmpDir </> "logs" </> "aiken-processes.log"
      _ <- withLogFile aikenLogFilePath $ \out -> do
        hSetBuffering out NoBuffering
        let aikenExec = proc "aiken" ["build"]
            aikenProcess = aikenExec{std_out = UseHandle out, std_err = UseHandle out}
        (_, _, _, aikenProcessHandle) <- createProcess aikenProcess
        waitForProcess aikenProcessHandle
      -- Run 'git status' to see if plutus.json file has changed
      let gitLogFilePath = tmpDir </> "logs" </> "git-processes.log"
      _ <- withLogFile gitLogFilePath $ \out -> do
        hSetBuffering out NoBuffering
        let gitStatusExec = proc "git" ["status", "--porcelain"]
            gitStatusProcess = gitStatusExec{std_out = UseHandle out, std_err = UseHandle out}
        (_, _, _, gitStatusProcessHandle) <- createProcess gitStatusProcess
        waitForProcess gitStatusProcessHandle
      -- Read git log file and verify plutus.json did not change
      gitLogContents <- decodeUtf8 <$> readFileBS gitLogFilePath
      gitLogContents `shouldNotContain` "plutus.json"
  it "Initial validator script" $
    goldenScript "vInitial" Initial.validatorScript
  it "Commit validator script" $
    -- TODO: the script is now double in the repo. Use plutus.json for a golden file
    goldenScript "vCommit" commitValidatorScript
  it "Head validator script" $
    goldenScript "vHead" Head.validatorScript
  it "Head minting policy script" $
    goldenScript "mHead" (serialiseCompiledCode HeadTokens.unappliedMintingPolicy)

-- | Write a golden script on first run and ensure it stays the same on
-- subsequent runs.
goldenScript :: String -> Plutus.SerialisedScript -> Golden Script
goldenScript name plutusScript =
  Golden
    { output = PlutusScript $ fromPlutusScript plutusScript
    , encodePretty = show . hashScript
    , writeToFile
    , readFromFile
    , goldenFile = "scripts/" <> name <> ".plutus"
    , actualFile = Nothing
    , failFirstTime = False
    }
 where
  fullScriptName = "hydra-" <> name <> maybe "" ("-" <>) gitDescribe

  writeToFile fp script =
    void $ writeFileTextEnvelope (File fp) (Just $ fromString fullScriptName) script

  readFromFile fp =
    either (die . show) pure
      =<< readFileTextEnvelope (AsScript AsPlutusScriptV2) (File fp)
