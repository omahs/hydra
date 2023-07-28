{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Tracer (stdoutTracer, traceWith)
import Hydra.Cardano.Api (AsType (AsSigningKey, AsVerificationKey), Tx)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey), sign)
import Hydra.Logging (Verbosity (..), withTracer)
import Hydra.Network (Host (..))
import Hydra.Network.Authenticate (Signed (..))
import Hydra.Network.Heartbeat (Heartbeat (Data))
import Hydra.Network.Message (Message (ReqSn))
import Hydra.Network.Ouroboros.Client (FireForgetClient (..), fireForgetClientPeer)
import Hydra.Network.Ouroboros.Type (codecFireForget)
import Hydra.Options (hydraSigningKeyFileParser, hydraVerificationKeyFileParser, peerParser)
import Hydra.Party (Party (..))
import Hydra.Prelude
import Hydra.Snapshot (SnapshotNumber (UnsafeSnapshotNumber))
import Log (NetLog (..))
import Network.Socket (
  AddrInfo (..),
  SocketType (Stream),
  connect,
  defaultHints,
  defaultProtocol,
  getAddrInfo,
  socket,
 )
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
 )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (MiniProtocolLimits, maximumIngressQueue), MiniProtocolNum (..), MuxPeer (..), OuroborosApplication (..), RunMiniProtocol (..))
import Ouroboros.Network.Protocol.Handshake.Codec (noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned (unversionedHandshakeCodec, unversionedProtocol, unversionedProtocolDataCodec)
import Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion)
import Ouroboros.Network.Socket (
  NetworkConnectTracers (..),
  connectToNodeSocket,
 )

data Options = InjectReqSn
  { peer :: Host
  -- ^ The host to connect to
  , snapshotNumber :: SnapshotNumber
  -- ^ The number of the snapshot to inject
  , hydraKey :: FilePath
  -- ^ The signing key to use for signing
  , fakeHydraKey :: FilePath
  -- ^ The verification key to impersonate
  }
  deriving stock (Show)

injectReqSnParser :: Parser Options
injectReqSnParser =
  InjectReqSn
    <$> peerParser
    <*> snapshotNumberParser
    <*> hydraSigningKeyFileParser
    <*> hydraVerificationKeyFileParser

snapshotNumberParser :: Parser SnapshotNumber
snapshotNumberParser =
  UnsafeSnapshotNumber
    <$> option
      auto
      ( long "snapshot-number"
          <> short 's'
          <> metavar "NATURAL"
          <> help
            "The number of the snapshot to craft a ReqSn for"
      )

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command
        "reqsn"
        ( info
            (helper <*> injectReqSnParser)
            (progDesc "Inject a ReqSn message for given number seemingly from another peer.")
        )
    )

netOptions :: ParserInfo Options
netOptions =
  info
    ( commandsParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Hydra Network Injector"
        <> header "hydra-net - CLI tool to inject messages into a Hydra nodes network"
    )

main :: IO ()
main =
  execParser netOptions >>= \case
    InjectReqSn{peer, snapshotNumber, hydraKey, fakeHydraKey} -> injectReqSn peer snapshotNumber hydraKey fakeHydraKey

injectReqSn :: Host -> SnapshotNumber -> FilePath -> FilePath -> IO ()
injectReqSn peer snapshotNumber hydraKeyFile fakeHydraKeyFile = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) hydraKeyFile
  party <- Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) fakeHydraKeyFile
  withIOManager $ \iomgr -> do
    withTracer (Verbose "hydra-net") $ \tracer -> do
      sockAddr <- resolveSockAddr peer
      sock <- socket (addrFamily sockAddr) Stream defaultProtocol
      traceWith tracer $ ConnectingTo sockAddr
      connect sock (addrAddress sockAddr)
      traceWith tracer $ ConnectedTo sockAddr
      runClient iomgr (mkApplication sk party tracer) sock
 where
  runClient iomgr app sock =
    connectToNodeSocket
      iomgr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      networkConnectTracers
      acceptableVersion
      (unversionedProtocol app)
      sock

  networkConnectTracers =
    NetworkConnectTracers
      { nctMuxTracer = contramap show stdoutTracer
      , nctHandshakeTracer = contramap show stdoutTracer
      }

  resolveSockAddr Host{hostname, port} = do
    is <- getAddrInfo (Just defaultHints) (Just $ toString hostname) (Just $ show port)
    case is of
      (inf : _) -> pure inf
      _ -> error "getAdrrInfo failed.. do proper error handling"

  mkApplication sk party tracer = OuroborosApplication $ \_connectionId _controlMessageSTM ->
    [ MiniProtocol
        { miniProtocolNum = MiniProtocolNum 42
        , miniProtocolLimits = MiniProtocolLimits{maximumIngressQueue = maxBound}
        , miniProtocolRun =
            InitiatorProtocolOnly
              ( MuxPeer
                  (contramap TraceSendRecv tracer)
                  codecFireForget
                  (fireForgetClientPeer $ client tracer sk party)
              )
        }
    ]

  client tracer sk party = Idle $ do
    let msg = Data "2" (ReqSn @Tx snapshotNumber [])
    let signed = Signed msg (sign sk msg) party
    traceWith tracer $ Injecting signed
    pure $ SendMsg signed (pure $ SendDone (pure ()))
