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
import Hydra.API.ClientInput (ClientInput)

--
-- Model
--
data FeedbackVerbosity = Short | Full

data State = State {
  nodeHost :: Host,
  now :: UTCTime,
  connectedState :: ConnectedState,
  feedbackState :: FeedbackVerbosity
}

data ConnectedState
  = Disconnected
  | Connected { connection :: Connection }

data IdentifiedState = Unidentified | Identified Party

data Connection = Connection
      { me :: IdentifiedState
      , peers :: [NodeId]
      , headState :: HeadState
      , feedback :: [UserFeedback]
      , transitionNote :: Maybe Text
      , hydraHeadId :: Maybe HeadId
      }

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
  [ ("connectedState", "connectedStateL")
  , ("nodeHost", "nodeHostL")
  , ("clientState", "clientStateL")
  , ("dialogState", "dialogStateL")
  , ("feedbackState", "feedbackStateL")
  , ("now", "nowL")
  , ("hydraHeadId", "hydraHeadIdL")
  ]
  ''State

makeLensesFor
  [ ("connection", "connectionL") ]
  ''ConnectedState

makeLensesFor
  [ ("transitionNote", "transitionNoteL")
  , ("me", "meL")
  , ("feedback", "feedbackL")
  , ("headState", "headStateL")
  , ("peers", "peersL")
  ]
  ''Connection

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
