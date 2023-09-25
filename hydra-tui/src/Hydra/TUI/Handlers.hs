{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Handlers where

import Hydra.Prelude hiding (Down, State, padLeft)

import Brick
import Hydra.Cardano.Api

import Brick.Forms (
  Form,
  FormFieldState,
  checkboxField,
  editShowableFieldWithValidate,
  newForm,
  radioField,
 )
import qualified Cardano.Api.UTxO as UTxO
import Data.List (nub, (\\))
import qualified Data.Map.Strict as Map
import Graphics.Vty (
  Event (EvKey),
  Key (..),
  Modifier (..),
 )
import qualified Graphics.Vty as Vty
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput (..))
import Hydra.Chain (PostTxError (..))
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (Client (..), HydraEvent (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Snapshot (Snapshot (..))
import Lens.Micro (Lens', lens)
import Lens.Micro.Mtl (preuse, (.=), assign, use, (%=), (?=))
import qualified Prelude
import Hydra.TUI.Model

info :: MonadState State m => Text -> m ()
info = report Info

info' :: MonadState State m => UTCTime -> Text -> m ()
info' time = report' time Info

warn :: MonadState State m => Text -> m ()
warn = report Error

warn' :: MonadState State m => UTCTime -> Text -> m ()
warn' time = report' time Error

report :: MonadState State m => Severity -> Text -> m ()
report typ msg = do
  x <- use nowL
  report' x typ msg

report' :: MonadState State m => UTCTime -> Severity -> Text -> m ()
report' time typ msg = feedbackL %= (UserFeedback typ msg time :)

stopPending :: MonadState Pending m => m ()
stopPending = id .= NotPending

initPending :: MonadState Pending m => m ()
initPending = id .= Pending

--
-- Update
--

handleVtyEventDisconnected :: Vty.Event -> EventM Name State ()
handleVtyEventDisconnected e = case e of
  EvKey (KChar 'c') [MCtrl] -> halt
  EvKey (KChar 'd') [MCtrl] -> halt
  EvKey (KChar 'q') [] -> halt
  _ -> pure ()

handleHydraEventDisconnected :: HydraEvent Tx -> EventM Name State ()
handleHydraEventDisconnected = \case
  ClientConnected -> do
     connectedStateL .= (Connected $ Connection
       { me = Unidentified
       , peers = []
       , headState = Idle
       , pending = NotPending
       , hydraHeadId = Nothing
       })
  _ -> pure ()

handleBrickEventDisconnected :: BrickEvent w (HydraEvent Tx) -> EventM Name State ()
handleBrickEventDisconnected = \case
  AppEvent e -> handleHydraEventDisconnected e
  VtyEvent e -> handleVtyEventDisconnected e
  _ -> pure ()

handleVtyEventConnected :: Vty.Event -> EventM Name State ()
handleVtyEventConnected _ = pure ()

handleHydraEventConnected :: HydraEvent Tx -> EventM Name State ()
handleHydraEventConnected = \case
  Update TimedServerOutput{output = Greetings{me}} -> connectedStateL . connectionL . meL .= Identified me
  _ -> pure ()

handleBrickEventConnected :: BrickEvent w (HydraEvent Tx) -> EventM Name State ()
handleBrickEventConnected = \case
  AppEvent e -> handleHydraEventDisconnected e
  VtyEvent e -> handleVtyEventDisconnected e
  _ -> pure ()


handleEvent ::
  Client Tx IO ->
  CardanoClient ->
  BrickEvent Name (HydraEvent Tx) ->
  EventM Name State ()
handleEvent client cardanoClient e = use connectedStateL >>= \case
    Disconnected -> handleBrickEventDisconnected e
    Connected c -> handleBrickEventConnected e

{--
  pure x

  AppEvent e -> handleAppEvent e
  VtyEvent e -> do
    x <- preuse dialogStateL
    case x of
      Just (Dialog title form) ->
        handleDialogEvent e
      Just NoDialog -> case e of
        -- Quit
        EvKey (KChar 'c') [MCtrl] -> halt
        EvKey (KChar 'd') [MCtrl] -> halt
        -- Commands
        EvKey (KChar c) _ ->
          if
              | (c Prelude.== '<') ->
                  zoom feedbackStateL $ scroll Up
              | c `elem` ['>'] ->
                  zoom feedbackStateL $ scroll Down
              | c `elem` ['h', 'H'] ->
                  feedbackStateL .= Full
              | c `elem` ['s', 'S'] ->
                  feedbackStateL .= Short
              | c `elem` ['q', 'Q'] ->
                  halt
              | c `elem` ['i', 'I'] ->
                  liftIO (sendInput client Init) >> setPending
              | c `elem` ['a', 'A'] ->
                  liftIO (sendInput client Abort) >> setPending
              | c `elem` ['f', 'F'] ->
                  liftIO (sendInput client Fanout) >> setPending
              | c `elem` ['c', 'C'] -> do
                  z <- preuse headStateL
                  case z of
                    Just Initializing{} ->
                      showCommitDialog client cardanoClient
                    Just Open{} ->
                      liftIO (sendInput client Close) >> setPending
                    _ ->
--                      pure ()
              | c `elem` ['n', 'N'] ->
                  pure ()
                  --handleNewTxEvent client cardanoClient
              | otherwise ->
                  pure ()
        _ -> pure ()
      -- Not connected
      Nothing -> case e of
        -- Quit
        EvKey (KChar 'c') [MCtrl] -> halt
        EvKey (KChar 'd') [MCtrl] -> halt
        EvKey (KChar 'q') [] -> halt
        _ -> pure ()
  e ->
    warn ("unhandled event: " <> show e)

setPending :: EventM n State ()
setPending = preuse pendingL >>= \case
    Just Pending -> info "Transition already pending"
    Just NotPending -> initPending
    Nothing -> pure ()

handleAppEvent ::
  MonadState State m =>
  HydraEvent Tx ->
  m ()
handleAppEvent = \case
  ClientConnected -> do
    n <- use nodeHostL
    t <- use nowL
    id .= Connected
      { nodeHost = n
      , me = Unidentified
      , peers = []
      , headState = Idle
      , dialogState = NoDialog
      , feedbackState = Short
      , feedback = []
      , now = t
      , pending = NotPending
      , hydraHeadId = Nothing
      }
  ClientDisconnected -> do
    n <- use nodeHostL
    t <- use nowL
    id .= Disconnected
      { nodeHost = n
      , now = t
      }
  Update TimedServerOutput{output = Greetings{me}} ->
    meL ?= me
  Update TimedServerOutput{output = PeerConnected p} ->
    peersL %= \cp -> nub $ cp <> [p]
  Update TimedServerOutput{output = (PeerDisconnected p)} ->
    peersL %= \cp -> cp \\ [p]
  Update TimedServerOutput{time, output = CommandFailed{clientInput}} -> do
    warn' time ("Invalid command: " <> show clientInput)
    stopPending
  Update TimedServerOutput{time, output = HeadIsInitializing{parties, headId}} -> do
    let utxo = mempty
        ps = toList parties

    headStateL .= Initializing{parties = ps, remainingParties = ps, utxo, headId = headId}
    stopPending
    info' time "Head initialized, ready for commit(s)."

  Update TimedServerOutput{output = Committed{party, utxo}} -> do
    headStateL %= partyCommitted [party] utxo
    info (show party <> " committed " <> renderValue (balance @Tx utxo))
    x <- preuse meL
    if Just (Just party) == x then stopPending else pure ()
  Update TimedServerOutput{time, output = HeadIsOpen{utxo}} -> do
      headStateL %= headIsOpen utxo
      info' time "Head is now open!"
  Update TimedServerOutput{time, output = HeadIsClosed{headId, snapshotNumber, contestationDeadline}} -> do
      headStateL .= Closed{headId, contestationDeadline}
      info' time ("Head closed with snapshot number " <> show snapshotNumber)
      stopPending
  Update TimedServerOutput{output = HeadIsContested{headId, snapshotNumber}} -> do
      info ("Head " <> show headId <> " contested with snapshot number " <> show snapshotNumber)
  Update TimedServerOutput{time, output = ReadyToFanout{headId}} -> do
      headStateL .= FanoutPossible{headId}
      info' time "Contestation period passed, ready for fanout."
  Update TimedServerOutput{time, output = HeadIsAborted{}} -> do
      headStateL .= Idle
      info' time "Head aborted, back to square one."
      stopPending
  Update TimedServerOutput{time, output = HeadIsFinalized{utxo}} -> do
      headStateL .= Final{utxo}
      info' time "Head finalized."
      stopPending
  Update TimedServerOutput{time, output = TxValid{}} -> do
      report' time Success "Transaction submitted successfully!"
  Update TimedServerOutput{time, output = TxInvalid{transaction, validationError}} ->
      warn' time ("Transaction with id " <> show (txId transaction) <> " is not applicable: " <> show validationError)
  Update TimedServerOutput{output = SnapshotConfirmed{snapshot}} ->
    snapshotConfirmed snapshot
  Update TimedServerOutput{output = GetUTxOResponse{}} ->
    pure ()
  Update TimedServerOutput{time, output = InvalidInput{reason}} ->
    warn' time ("Invalid input error: " <> toText reason)
  Update TimedServerOutput{time, output = PostTxOnChainFailed{postTxError}} ->
    case postTxError of
      NotEnoughFuel -> do
        warn' time "Not enough Fuel. Please provide more to the internal wallet and try again."
        stopPending
      InternalWalletError{reason} -> do
        warn' time reason
        stopPending
      _ -> do
          warn' time ("An error happened while trying to post a transaction on-chain: " <> show postTxError)
          stopPending
  Tick now ->
    nowL .= now
 where
  partyCommitted party commit = \case
    Initializing{parties, remainingParties, utxo, headId} ->
      Initializing
        { parties = parties
        , remainingParties = remainingParties \\ party
        , utxo = utxo <> commit
        , headId
        }
    hs -> hs

  headIsOpen utxo = \case
    Initializing{headId, parties} -> Open{headId, parties, utxo}
    hs -> hs

  snapshotConfirmed Snapshot{utxo, number} = do
    x <- preuse headStateL
    case x of
      Just Open{} -> do
          headStateL . utxoL .= utxo
          info ("Snapshot #" <> show number <> " confirmed.")
      _ ->
        warn "Snapshot confirmed but head is not open?"

handleDialogEvent :: Vty.Event -> EventM n State ()
handleDialogEvent x = do
  zoom feedbackStateL $ handleFeedbackVerbosityHotkey x

-- NOTE: Field focus is changed using Tab / Shift-Tab, but arrows are more
-- intuitive, so we forward them. Same for Space <-> Enter

handleFocusHotkeys :: Vty.Event -> EventM n State ()
handleFocusHotkeys = \case
    EvKey KUp [] ->
      handleDialogEvent $ EvKey KBackTab []
    EvKey KDown [] ->
      handleDialogEvent $ EvKey (KChar '\t') []
    _ -> pure ()

handleFeedbackScroll :: Vty.Event -> EventM Name FeedbackVerbosity ()
handleFeedbackScroll = \case
  EvKey (KChar c) _
    | c `elem` ['<'] -> scroll Up
    | c `elem` ['>'] -> scroll Down
  _ -> pure ()

handleFeedbackVerbosityHotkey :: Vty.Event -> EventM n FeedbackVerbosity ()
handleFeedbackVerbosityHotkey = \case
  EvKey (KChar c) _
    | c `elem` ['h', 'H'] -> id .= Full
    | c `elem` ['s', 'S'] -> id .= Short
  _ -> pure ()

showCommitDialog ::
  Client Tx IO ->
  CardanoClient ->
  EventM n State ()
showCommitDialog Client{sk, externalCommit} CardanoClient{queryUTxOByAddress, networkId} = do
  utxo <- liftIO $ queryUTxOByAddress [ourAddress]
  dialogStateL .= commitDialog (UTxO.toMap utxo)
 where
  ourAddress =
    makeShelleyAddress
      networkId
      (PaymentCredentialByKey . verificationKeyHash $ getVerificationKey sk)
      NoStakeAddress

  commitDialog u =
    Dialog title form
   where
    title = "Select UTXO to commit"
    form = newForm (utxoCheckboxField u) ((,False) <$> u)
    submit selected = do
      let commitUTxO = UTxO $ Map.mapMaybe (\(v, p) -> if p then Just v else Nothing) selected
      liftIO (externalCommit commitUTxO) >> do
        dialogStateL .= NoDialog
        setPending

mkTransactionBuilderForm
  :: MonadState State m =>
   MonadReader AppEnv m =>
  m (Form (TxIn, TxOut CtxUTxO) w Name)
mkTransactionBuilderForm = do
   x <- myAvailableUTxO
   pure $ newForm (utxoRadioField x) (Prelude.head (Map.toList x))

askVerificationKey :: MonadReader AppEnv m => m (VerificationKey PaymentKey)
askVerificationKey = do
  x <- asks hydraClient
  pure $ getVerificationKey $ sk x

askNetworkId :: MonadReader AppEnv m => m NetworkId
askNetworkId = do
  x <- asks cardanoClient
  pure $ networkId x

mkTransactionBuilderDialog
  :: forall m. MonadState State m =>
     MonadReader AppEnv m =>
     UTxO
  -> m DialogState
mkTransactionBuilderDialog utxo = do
  let title :: Text
      title = "Select UTXO to spend"

      submit :: m ()
      submit = mkRecipientsDialog utxo >>= assign dialogStateL

  form <- mkTransactionBuilderForm

  pure $ Dialog title form

mkRecipientsForm :: UTxO -> Form AddressInEra w Name
mkRecipientsForm (UTxO utxo) =
  let field = radioField id [(u, show u, decodeUtf8 $ encodePretty u) | u <- nub addresses]
      addresses = getRecipientAddress <$> Map.elems utxo
      getRecipientAddress TxOut{txOutAddress = addr} = addr
  in newForm [field] (Prelude.head addresses)


mkRecipientsDialog :: forall m. MonadState State m => UTxO -> m DialogState
mkRecipientsDialog input = do
   let title :: Text
       title = "Select a recipient"

       submit :: MonadReader AppEnv m => AddressInEra -> m ()
       submit recipient = mkAmountDialog (Prelude.head (UTxO.pairs input)) recipient >>= assign dialogStateL

       form :: Form AddressInEra w Name
       form = mkRecipientsForm input

   pure $ Dialog title form

-- NOTE(SN): use 'Integer' because we don't have a 'Read Lovelace'
mkAmountForm :: Integer -> Form Integer w Name
mkAmountForm limit =
  let field = editShowableFieldWithValidate id "amount" (\n -> n > 0 && n <= limit)
  in newForm [field] limit

mkAmountDialog :: MonadReader AppEnv m => (TxIn, TxOut CtxUTxO) -> AddressInEra -> m DialogState
mkAmountDialog input@(_, x) recipient = do

   sk <- asks (sk . hydraClient)

   sendInput <- asks (sendInput . hydraClient)

   let title :: Text
       title = "Choose an amount (max: " <> show limit <> ")"

       limit :: Integer
       Lovelace limit = selectLovelace (txOutValue x)

       submit :: MonadState State m => MonadIO m => Integer -> m ()
       submit amount = do
         case mkSimpleTx input (recipient, lovelaceToValue $ Lovelace amount) sk of
          Left e -> warn ("Failed to construct tx, contact @_ktorz_ on twitter: " <> show e)
          Right tx -> do
            liftIO (sendInput (NewTx tx))
            dialogStateL .= NoDialog

       form :: Form Integer w Name
       form = mkAmountForm limit

   pure $ Dialog title form


data AppEnv = AppEnv {
    hydraClient :: Client Tx IO
  , cardanoClient :: CardanoClient
}


handleNewTxEvent ::
  MonadState State m =>
  MonadReader AppEnv m =>
  m ()
handleNewTxEvent = do
  x <- preuse headStateL
  case x of
    Just Open{utxo} ->
      mkTransactionBuilderDialog utxo >>= assign dialogStateL
    _ -> warn "Invalid command."


--
-- View
--
scroll :: Direction -> EventM Name FeedbackVerbosity ()
scroll direction = do
  x <- use id
  case x of
    Full -> do
      let vp = viewportScroll fullFeedbackViewportName
      vScrollPage vp direction
    Short -> do
      let vp = viewportScroll shortFeedbackViewportName
      hScrollPage vp direction

--
-- Forms additional widgets
--

-- A helper for creating multiple form fields from a UTXO set.
utxoCheckboxField ::
  forall s e n.
  ( s ~ Map.Map TxIn (TxOut CtxUTxO, Bool)
  , n ~ Name
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  [s -> FormFieldState s e n]
utxoCheckboxField u =
  [ checkboxField
    (checkboxLens k)
    ("checkboxField@" <> show k)
    (UTxO.render (k, v))
  | (k, v) <- Map.toList u
  ]
 where
  checkboxLens :: Ord k => k -> Lens' (Map k (v, Bool)) Bool
  checkboxLens i =
    lens
      (maybe False snd . Map.lookup i)
      (\s b -> Map.adjust (second (const b)) i s)

-- A helper for creating a radio form fields for selecting a UTXO in a given set
utxoRadioField ::
  forall s e n.
  ( s ~ (TxIn, TxOut CtxUTxO)
  , n ~ Name
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  [s -> FormFieldState s e n]
utxoRadioField u =
  [ radioField
      id
      [ (i, show i, UTxO.render i)
      | i <- Map.toList u
      ]
  ]

myAvailableUTxO :: MonadReader AppEnv m => MonadState State m => m (Map TxIn (TxOut CtxUTxO))
myAvailableUTxO = do
  x <- preuse headStateL
  networkId <- askNetworkId
  vk <- askVerificationKey
  case x of
    Just Open{utxo = UTxO u'} ->
      let myAddress = mkVkAddress networkId vk
       in pure $ Map.filter (\TxOut{txOutAddress = addr} -> addr == myAddress) u'
    _ ->
      pure mempty
--}
