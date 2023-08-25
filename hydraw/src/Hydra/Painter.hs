{-# LANGUAGE TypeApplications #-}

module Hydra.Painter where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Chain.Direct.State ()

data Pixel = Pixel
  { x, y, red, green, blue :: Word8
  }
-- | Create a zero-fee, payment cardano transaction with pixel metadata, which
-- just re-spends the given UTxO.
mkPaintTx ::
  -- | UTxO to spend
  (TxIn, TxOut CtxUTxO) ->
  -- | Signing key which owns the UTxO.
  SigningKey PaymentKey ->
  Pixel ->
  Either TxBodyError Tx
mkPaintTx (txin, txOut) sk Pixel{x, y, red, green, blue} = do
  body <- createAndValidateTransactionBody bodyContent
  pure $ signShelleyTransaction body [WitnessPaymentKey sk]
 where
  bodyContent =
    defaultTxBodyContent
      & addTxIn (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)
      & addTxOut (toTxContext txOut)
      & setTxFee (TxFeeExplicit $ Lovelace 0)
      & setTxMetadata metadata

  metadata = TxMetadataInEra $ TxMetadata $ fromList [(14, listOfInts)]

  listOfInts = TxMetaList $ TxMetaNumber . fromIntegral <$> [x, y, red, green, blue]
