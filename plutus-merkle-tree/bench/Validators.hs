{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Validators where

import PlutusTx.Prelude

import Plutus.Extras (wrapValidator)
import Plutus.MerkleTree (member)
import qualified Plutus.MerkleTree as MT
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V2 (ScriptContext)
import qualified PlutusTx as Plutus

-- | A validator for measuring cost of MT membership validation.
merkleTreeMemberValidator :: SerialisedScript
merkleTreeMemberValidator =
  serialiseCompiledCode
    $$( Plutus.compile
          [||
          wrapValidator $
            \() (e, root, proof) (_ :: ScriptContext) ->
              member e root proof
          ||]
      )

-- | A validator for measuring cost of MT construction.
-- data MtBuilderValidator
merkleTreeBuilderValidator :: SerialisedScript
merkleTreeBuilderValidator =
  serialiseCompiledCode
    $$( Plutus.compile
          [||
          wrapValidator $
            \() (utxos, root) (_ :: ScriptContext) ->
              MT.rootHash (MT.fromList utxos) == root
          ||]
      )
