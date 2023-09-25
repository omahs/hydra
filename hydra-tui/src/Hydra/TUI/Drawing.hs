{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Drawing where

import Hydra.Prelude hiding (Down, State, padLeft)

import Brick
import Hydra.Cardano.Api

import Hydra.Network (NodeId)
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
import Data.Version (showVersion, Version)
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
import Hydra.Chain (HeadId)

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
draw Client{sk} CardanoClient{networkId} s = pure $ withBorderStyle ascii $ joinBorders $ vBox
  [ hBox
    [ hLimit 50 $ vBox
      [ drawTUIVersion version <+> padLeft (Pad 1) (drawConnectedStatus s)
      , drawPeersIfConnected (s ^. connectedStateL)
      , hBorder
      , drawIfConnected (drawMeIfIdentified . me) (s ^. connectedStateL)
      , drawMyAddress $ mkVkAddress networkId (getVerificationKey sk)
      ]
    , vBorder
    , vBox [
        drawHeadState (s ^. connectedStateL),
        hBorder,
        hLimit 50 $ padLeftRight 1 $ drawCommandList s
      ]
    ]
  , maybeWidget drawUserFeedbackShort (s ^? connectedStateL . connectionL . feedbackL . _head)
  ]

drawCommandList :: State -> Widget n
drawCommandList s = vBox . fmap txt $ case s ^. connectedStateL of
  Disconnected -> ["[Q]uit"]
  Connected c -> case c ^. headStateL of
    Idle -> ["[I]nit", "[Q]uit"]
    Initializing{} -> ["[C]ommit", "[A]bort", "[Q]uit"]
    Open{} -> ["[N]ew Transaction", "[C]lose", "[Q]uit"]
    Closed{} -> ["[Q]uit"]
    FanoutPossible{} -> ["[F]anout", "[Q]uit"]
    Final{} -> ["[I]nit", "[Q]uit"]

{--
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
      , let cmds =
              [ "[<] scroll up"
              , "[>] scroll down"
              , "[S]hort Feedback Mode"
              ]
         in hBox
              [ hLimit 150 $ viewport fullFeedbackViewportName Vertical drawFullFeedback
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
        [ padLeftRight 1 $ drawTUIVersion version <+> padLeft (Pad 1) nodeStatus
        , padLeftRight 1 drawPeersIfConnected
        , hBorder
        , padLeftRight 1 ownParty
        , padLeftRight 1 ownAddress
        , padLeftRight 1 $ maybeWidget drawPartiesWithOwnHighlighted (s ^? headStateL . partiesL)
        ]
   where

    ownParty =
      case s ^? meL of
        Just (Just Party{vkey}) -> str "Party " <+> withAttr own (txt $ serialiseToRawBytesHexText vkey)
        _ -> emptyWidget

    ownAddress =
      str "Address " <+> drawAddress (mkVkAddress networkId vk)
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
                      <=> vBox (map drawPartyWithOwnHighlighted remainingParties)
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
              , maybeWidget drawHeadId (headState ^? headIdL)
              ]
        , hBorder
        ]

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


  withCommands panel cmds =
    hBox
      [ hLimit 100 (vBox panel)
      , vBorder
      , padLeftRight 1 $ vBox (str <$> cmds)
      ]

  drawFullFeedback :: Widget n
  drawFullFeedback = vBox $
    case s ^? feedbackL of
      Just feedbacks -> drawUserFeedbackFull <$> feedbacks
       where
      Nothing ->
        -- Reserves the space and not have this area collapse
        [txt ""]

  drawShortFeedback :: Widget n
  drawShortFeedback =
    case s ^? (feedbackL . _head) of
      Just x -> drawUserFeedbackShort x
      Nothing ->
        -- Reserves the space and not have this area collapse
        str ""

  drawPartyWithOwnHighlighted :: Party -> Widget n
  drawPartyWithOwnHighlighted p = drawParty (if s ^? meL == Just (Just p) then own else mempty) p

  drawPartiesWithOwnHighlighted :: [Party] -> Widget n
  drawPartiesWithOwnHighlighted = drawParties drawPartyWithOwnHighlighted

--}

drawUserFeedbackFull :: UserFeedback -> Widget n
drawUserFeedbackFull UserFeedback{message, severity, time} =
  let feedbackText = show time <> " | " <> message
      feedbackChunks = chunksOf 150 feedbackText
      feedbackDecorator = withAttr (severityToAttr severity) . txt
  in vBox $ fmap feedbackDecorator feedbackChunks

drawUserFeedbackShort :: UserFeedback -> Widget n
drawUserFeedbackShort (UserFeedback{message, severity, time}) =
  withAttr (severityToAttr severity) . str . toString $ (show time <> " | " <> message)

drawParties :: (Party -> Widget n) -> [Party] -> Widget n
drawParties f xs = vBox $ str "Head participants:" : map f xs


drawIfConnected :: (Connection -> Widget n) -> ConnectedState -> Widget n
drawIfConnected f = \case
  Disconnected{} -> emptyWidget
  Connected c -> f c

drawPeersIfConnected :: ConnectedState -> Widget n
drawPeersIfConnected = drawIfConnected (drawPeers . peers)

drawHeadId :: HeadId -> Widget n
drawHeadId x = txt $ "Head id: " <> serialiseToRawBytesHexText x


drawMyAddress :: AddressInEra -> Widget n
drawMyAddress addr = str "Address " <+> withAttr own (drawAddress addr)

drawAddress :: AddressInEra -> Widget n
drawAddress addr = txt (ellipsize 40 $ serialiseAddress addr)

ellipsize :: Int -> Text -> Text
ellipsize n t = Text.take (n - 2) t <> ".."

drawMeIfIdentified :: IdentifiedState -> Widget n
drawMeIfIdentified (Identified Party{vkey}) = str "Party " <+> withAttr own (txt $ serialiseToRawBytesHexText vkey)
drawMeIfIdentified Unidentified = emptyWidget


drawConnectedStatus :: State -> Widget n
drawConnectedStatus State{nodeHost,connectedState} = case connectedState of
  Disconnected -> withAttr negative $ str $ "connecting to " <> show nodeHost
  Connected _ -> withAttr positive $ str $ "connected to " <> show nodeHost

drawParty :: AttrName -> Party -> Widget n
drawParty x Party{vkey} = withAttr x $ drawHex vkey

drawPeers :: [NodeId] -> Widget n
drawPeers peers = vBox $ str "Peers connected to our node:" : map drawShow peers

maybeWidget :: (a -> Widget n) -> Maybe a -> Widget n
maybeWidget = maybe emptyWidget

drawTUIVersion :: Version -> Widget n
drawTUIVersion v = str "Hydra TUI " <+> str (showVersion v)

drawHex :: SerialiseAsRawBytes a => a -> Widget n
drawHex = txt . (" - " <>) . serialiseToRawBytesHexText

drawShow :: forall a n. Show a => a -> Widget n
drawShow = txt . (" - " <>) . show

renderTime :: (Ord t, Num t, FormatTime t) => t -> String
renderTime r
  | r < 0 = "-" <> renderTime (negate r)
  | otherwise = formatTime defaultTimeLocale "%dd %Hh %Mm %Ss" r

drawHeadState :: ConnectedState -> Widget n
drawHeadState = \case
  Disconnected{} -> emptyWidget
  Connected(Connection{headState, pending = NotPending}) -> drawVBox headState $ txt ""
  Connected(Connection{headState, pending = Pending}) -> drawVBox headState $ txt " (Transition pending)"
  where
   drawVBox headState drawPending =
      vBox
        [ padLeftRight 1 $
            vBox
              [ txt "Head status: "
                  <+> withAttr infoA (txt $ Prelude.head (words $ show headState))
                  <+> drawPending
              , maybeWidget drawHeadId (headState ^? headIdL)
              ]
        , hBorder
          ]
--
-- Style
--
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
