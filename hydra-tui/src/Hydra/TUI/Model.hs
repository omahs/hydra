{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Model where

import Hydra.Prelude hiding (Down, State, padLeft)

import Hydra.Cardano.Api

import Brick.Forms (Form)
import Hydra.Chain (HeadId)
import Hydra.Chain.Direct.State ()
import Hydra.Client (HydraEvent (..))
import Hydra.Network (Host (..), NodeId)
import Hydra.Party (Party (..))
import Lens.Micro.TH (makeLensesFor)

--
-- Model
--
data FeedbackVerbosity = Short | Full

data State
  = Disconnected
      { nodeHost :: Host
      , now :: UTCTime
      }
  | Connected
      { me :: Maybe Party -- TODO(SN): we could make a nicer type if ClientConnected is only emited of 'Hydra.Client' upon receiving a 'Greeting'
      , nodeHost :: Host
      , peers :: [NodeId]
      , headState :: HeadState
      , dialogState :: DialogState
      , feedbackState :: FeedbackVerbosity
      , feedback :: [UserFeedback]
      , now :: UTCTime
      , pending :: Pending
      , hydraHeadId :: Maybe HeadId
      }

data Pending = Pending | NotPending deriving (Eq, Show, Generic)

data UserFeedback = UserFeedback
  { severity :: Severity
  , message :: Text
  , time :: UTCTime
  }
  deriving (Eq, Show, Generic)

data Severity
  = Success
  | Info
  | Error
  deriving (Eq, Show, Generic)

data DialogState where
  NoDialog :: DialogState
  Dialog ::
    forall s.
    Text ->
    Form s (HydraEvent Tx) Name ->
    DialogState

data HeadState
  = Idle
  | Initializing
      { parties :: [Party]
      , remainingParties :: [Party]
      , utxo :: UTxO
      , headId :: HeadId
      }
  | Open
      { parties :: [Party]
      , utxo :: UTxO
      , headId :: HeadId
      }
  | Closed
      { contestationDeadline :: UTCTime
      , headId :: HeadId
      }
  | FanoutPossible {headId :: HeadId}
  | Final {utxo :: UTxO}
  deriving (Eq, Show, Generic)

type Name = Text

makeLensesFor
  [ ("me", "meL")
  , ("nodeHost", "nodeHostL")
  , ("peers", "peersL")
  , ("headState", "headStateL")
  , ("clientState", "clientStateL")
  , ("dialogState", "dialogStateL")
  , ("feedback", "feedbackL")
  , ("feedbackState", "feedbackStateL")
  , ("now", "nowL")
  , ("pending", "pendingL")
  , ("hydraHeadId", "hydraHeadIdL")
  ]
  ''State

makeLensesFor
  [ ("remainingParties", "remainingPartiesL")
  , ("parties", "partiesL")
  , ("utxo", "utxoL")
  , ("headId", "headIdL")
  ]
  ''HeadState

fullFeedbackViewportName :: Name
fullFeedbackViewportName = "full-feedback-view-port"

shortFeedbackViewportName :: Name
shortFeedbackViewportName = "short-feedback-view-port"
