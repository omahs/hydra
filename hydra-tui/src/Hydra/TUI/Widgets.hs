{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Widgets where

import Hydra.Prelude hiding (Down, State, padLeft)

import Brick
import Hydra.Cardano.Api

import Hydra.TUI.Model (Peers(..))

drawHex :: SerialiseAsRawBytes a => a -> Widget n
drawHex = txt . (" - " <>) . serialiseToRawBytesHexText

drawShow :: forall a n. Show a => a -> Widget n
drawShow = txt . (" - " <>) . show

drawPeers :: Peers -> Widget n
drawPeers (Peers xs) = vBox $ str "Peers connected to our node:" : map drawShow xs
