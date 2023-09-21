{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Drawing where

import Hydra.Prelude hiding (Down, State, padLeft)

import Brick
import Hydra.Cardano.Api

import Brick.Forms (
  focusedFormInputAttr,
  invalidFormInputAttr,
  renderForm,
 )
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map.Strict as Map
import Data.Text (chunksOf)
import qualified Data.Text as Text
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Format (FormatTime)
import Data.Version (showVersion)
import Graphics.Vty (
  brightBlue,
  green,
  red,
  yellow,
 )
import Graphics.Vty.Attributes (defAttr)
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (Client (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Party (Party (..))
import Lens.Micro ((^.), (^?), _head)
import Paths_hydra_tui (version)
import qualified Prelude
import Hydra.TUI.Model

severityToAttr :: Severity -> AttrName
severityToAttr = \case
  Success -> positive
  Info -> infoA
  Error -> negative

infoA :: AttrName
infoA = attrName "info"

positive :: AttrName
positive = attrName "positive"

negative :: AttrName
negative = attrName "negative"

own :: AttrName
own = attrName "own"

draw :: Client Tx m -> CardanoClient -> State -> [Widget Name]
draw Client{sk} CardanoClient{networkId} s =
  pure $
    withBorderStyle ascii $
      joinBorders $
        case s ^? feedbackStateL of
          Just Full -> drawFullHistoryMode
          _ -> drawShortFeedbackMode
 where
  vk = getVerificationKey sk

  drawFullHistoryMode =
    vBox
      [ drawHeadState
      , let panel = drawFullFeedback
            cmds =
              [ "[<] scroll up"
              , "[>] scroll down"
              , "[S]hort Feedback Mode"
              ]
         in hBox
              [ hLimit 150 $ viewport fullFeedbackViewportName Vertical (vBox panel)
              , vBorder
              , vBox
                  [ padLeftRight 1 . vBox $ (str <$> commandList)
                  , hBorder
                  , padLeftRight 1 . vBox $ (str <$> cmds)
                  ]
              ]
      ]

  drawShortFeedbackMode =
    vBox
      [ hBox
          [ drawInfo
          , vBorder
          , drawRightPanel
          ]
      , hBorder
      , let panel = drawShortFeedback
            cmds =
              [ "[<] scroll left"
              , "[>] scroll right"
              , "Full [H]istory Mode"
              ]
         in vLimit 3 $
              hBox
                [ hLimit 150 $ viewport shortFeedbackViewportName Horizontal panel
                , vBorder
                , padLeftRight 1 . vBox $ (str <$> cmds)
                ]
      ]

  drawInfo =
    hLimit 50 $
      vBox
        [ padLeftRight 1 $ tuiVersion <+> padLeft (Pad 1) nodeStatus
        , padLeftRight 1 drawPeers
        , hBorder
        , padLeftRight 1 ownParty
        , padLeftRight 1 ownAddress
        , padLeftRight 1 drawParties
        ]
   where
    tuiVersion = str "Hydra TUI " <+> str (showVersion version)

    ownParty =
      case s ^? meL of
        Just (Just Party{vkey}) -> str "Party " <+> withAttr own (txt $ serialiseToRawBytesHexText vkey)
        _ -> emptyWidget

    ownAddress =
      str "Address " <+> drawAddress (mkVkAddress networkId vk)

    nodeStatus =
      case s of
        Disconnected{nodeHost} -> withAttr negative $ str $ "connecting to " <> show nodeHost
        Connected{nodeHost} -> withAttr positive $ str $ "connected to " <> show nodeHost

  commandList =
    case s ^? dialogStateL of
      Just Dialog{} -> ["[Esc] Cancel", "[↑] Move Up", "[↓] Move Down", "[Space] Select", "[Enter] Confirm"]
      _ ->
        case s ^? headStateL of
          Just Idle -> ["[I]nit", "[Q]uit"]
          Just Initializing{} -> ["[C]ommit", "[A]bort", "[Q]uit"]
          Just Open{} -> ["[N]ew Transaction", "[C]lose", "[Q]uit"]
          Just Closed{} -> ["[Q]uit"]
          Just FanoutPossible{} -> ["[F]anout", "[Q]uit"]
          Just Final{} -> ["[I]nit", "[Q]uit"]
          Nothing -> ["[Q]uit"]

  drawRightPanel =
    case s ^? dialogStateL of
      Just (Dialog title form) ->
        withCommands
          [ drawHeadState
          , padLeftRight 1 $ str (toString title)
          , padLeftRight 1 $ padTop (Pad 1) $ renderForm form
          ]
          [ "[Esc] Cancel"
          , "[↑] Move Up"
          , "[↓] Move Down"
          , "[Space] Select"
          , "[Enter] Confirm"
          ]
      _ ->
        case s ^? headStateL of
          Just Idle ->
            withCommands
              [drawHeadState]
              commandList
          Just Initializing{remainingParties, utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $ str ("Total committed: " <> toString (renderValue (balance @Tx utxo)))
              , padLeftRight 1 $
                  padTop (Pad 1) $
                    str "Waiting for parties to commit:"
                      <=> vBox (map drawParty remainingParties)
              ]
              commandList
          Just Open{utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $
                  txt ("Head UTXO, total: " <> renderValue (balance @Tx utxo))
                    <=> padLeft (Pad 2) (drawUTxO utxo)
              ]
              commandList
          Just Closed{contestationDeadline} ->
            withCommands
              [ drawHeadState
              , drawRemainingContestationPeriod contestationDeadline
              ]
              commandList
          Just FanoutPossible{} ->
            withCommands
              [ drawHeadState
              , txt "Ready to fanout!"
              ]
              commandList
          Just Final{utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $
                  txt ("Distributed UTXO, total: " <> renderValue (balance @Tx utxo))
                    <=> padLeft (Pad 2) (drawUTxO utxo)
              ]
              commandList
          -- Disconnected
          Nothing ->
            withCommands
              [ drawHeadState
              ]
              commandList

  drawRemainingContestationPeriod deadline =
    let remaining = diffUTCTime deadline (s ^. nowL)
     in if remaining > 0
          then padLeftRight 1 $ txt "Remaining time to contest: " <+> str (renderTime remaining)
          else txt "Contestation period passed, ready to fan out soon."

  drawHeadState = case s of
    Disconnected{} -> emptyWidget
    Connected{headState, pending = NotPending} -> drawVBox headState $ txt ""
    Connected{headState, pending = Pending} -> drawVBox headState $ txt " (Transition pending)"
   where
    drawVBox headState drawPending =
      vBox
        [ padLeftRight 1 $
            vBox
              [ txt "Head status: "
                  <+> withAttr infoA (txt $ Prelude.head (words $ show headState))
                  <+> drawPending
              , drawHeadId (headState ^? headIdL)
              ]
        , hBorder
        ]

    drawHeadId = \case
      Nothing -> emptyWidget
      Just headId -> txt $ "Head id: " <> serialiseToRawBytesHexText headId

  drawUTxO utxo =
    let byAddress =
          Map.foldrWithKey
            (\k v@TxOut{txOutAddress = addr} -> Map.unionWith (++) (Map.singleton addr [(k, v)]))
            mempty
            $ UTxO.toMap utxo
     in vBox
          [ padTop (Pad 1) $
            vBox
              [ drawAddress addr
              , padLeft (Pad 2) $ vBox (str . toString . UTxO.render <$> u)
              ]
          | (addr, u) <- Map.toList byAddress
          ]

  drawAddress addr
    | mkVkAddress networkId vk == addr =
        withAttr own widget
    | otherwise =
        widget
   where
    widget = txt $ ellipsize 40 $ serialiseAddress addr

  ellipsize n t = Text.take (n - 2) t <> ".."

  withCommands panel cmds =
    hBox
      [ hLimit 100 (vBox panel)
      , vBorder
      , padLeftRight 1 $ vBox (str <$> cmds)
      ]

  drawFullFeedback =
    case s ^? feedbackL of
      Just feedbacks -> vBox . feedbackToWidget <$> feedbacks
       where
        feedbackToWidget =
          ( \UserFeedback{message, severity, time} ->
              let feedbackText = show time <> " | " <> message
                  feedbackChunks = chunksOf 150 feedbackText
                  feedbackDecorator = withAttr (severityToAttr severity) . txt
               in feedbackDecorator <$> feedbackChunks
          )
      Nothing ->
        -- Reserves the space and not have this area collapse
        [txt ""]

  drawShortFeedback =
    case s ^? (feedbackL . _head) of
      Just UserFeedback{message, severity, time} ->
        withAttr (severityToAttr severity) . str . toString $ (show time <> " | " <> message)
      Nothing ->
        -- Reserves the space and not have this area collapse
        str ""

  drawParties =
    case s ^? headStateL . partiesL of
      Nothing -> emptyWidget
      Just ps -> vBox $ str "Head participants:" : map drawParty ps

  drawParty p@Party{vkey} =
    case s ^? meL of
      Just (Just me) | p == me -> withAttr own $ drawHex vkey
      _ -> drawHex vkey

  drawPeers = case s of
    Disconnected{} -> emptyWidget
    Connected{peers} -> vBox $ str "Peers connected to our node:" : map drawShow peers

  drawHex :: SerialiseAsRawBytes a => a -> Widget n
  drawHex = txt . (" - " <>) . serialiseToRawBytesHexText

  drawShow :: forall a n. Show a => a -> Widget n
  drawShow = txt . (" - " <>) . show

renderTime :: (Ord t, Num t, FormatTime t) => t -> String
renderTime r
  | r < 0 = "-" <> renderTime (negate r)
  | otherwise = formatTime defaultTimeLocale "%dd %Hh %Mm %Ss" r

--
-- Style
--
--}
style :: State -> AttrMap
style _ =
  attrMap
    defAttr
    [ (infoA, fg brightBlue)
    , (negative, fg red)
    , (positive, fg green)
    , (own, fg yellow)
    , -- Brick forms
      (focusedFormInputAttr, fg brightBlue)
    , (invalidFormInputAttr, fg red)
    ]
