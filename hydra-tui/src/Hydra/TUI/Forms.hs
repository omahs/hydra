{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Forms where

import Hydra.Prelude hiding (Down, State, padLeft)

import Hydra.Cardano.Api

import Brick.Forms (
  Form,
  checkboxField,
  newForm,
  radioField,
 )
import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map.Strict as Map
import Hydra.Chain.Direct.State ()
import Lens.Micro (Lens', lens)
import qualified Prelude

utxoCheckboxField ::
  forall s e n.
  ( s ~ Map.Map TxIn (TxOut CtxUTxO, Bool)
  , n ~ Text
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  Form s e n
utxoCheckboxField u = newForm
  [ checkboxField
    (checkboxLens k)
    ("checkboxField@" <> show k)
    (UTxO.render (k, v))
  | (k, v) <- Map.toList u
  ] ((,False) <$> u)
 where
  checkboxLens :: Ord k => k -> Lens' (Map k (v, Bool)) Bool
  checkboxLens i =
    lens
      (maybe False snd . Map.lookup i)
      (\s b -> Map.adjust (second (const b)) i s)

utxoRadioField ::
  forall s e n.
  ( s ~ (TxIn, TxOut CtxUTxO)
  , n ~ Text
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  Form s e n
utxoRadioField u = newForm
  [ radioField
      id
      [ (i, show i, UTxO.render i)
      | i <- Map.toList u
      ]
  ] (Prelude.head $ Map.toList u)
