module Main where

import Hydra.Prelude

import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..))
import Hydra.Network (Host, readHost)
import Hydra.Painter (Pixel (..), paintPixel, withClient)
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.Wai (
  Application,
  pathInfo,
  requestMethod,
  responseFile,
  responseLBS,
 )
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as Wai
import Network.WebSockets (Connection, PendingConnection)
import qualified Network.WebSockets as WS
import Safe (readMay)

main :: IO ()
main = do
  key <- requireEnv "HYDRAW_CARDANO_SIGNING_KEY"
  host <- parseHost =<< requireEnv "HYDRA_API_HOST"
  let networkId = Testnet (NetworkMagic 1)
  withClient host $ \con ->
    Warp.runSettings settings $
      Wai.websocketsOr
        WS.defaultConnectionOptions
        (proxyWebsocket host)
        (httpApp networkId key con)
 where
  port = 1337

  settings =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost "0.0.0.0"
      & Warp.setBeforeMainLoop
        ( do
            putStrLn "Server started..."
            putStrLn $ "Listening on: tcp/" <> show port
        )

  parseHost str =
    case readHost str of
      Nothing -> fail $ "Could not parse host address: " <> str
      Just host -> pure host

-- | Like 'lookupEnv' but terminate program with a message if environment
-- variable is not set.
requireEnv :: String -> IO String
requireEnv name =
  lookupEnv name >>= \case
    Just value -> pure value
    Nothing -> die $ "Error: Required environment variable " <> name <> " not set"

-- | Accept an incoming websocket connect, connect to hydra-node and forward all
-- messages (in both directions)
proxyWebsocket :: Host -> PendingConnection -> IO ()
proxyWebsocket host pendingConnection = do
  frontend <- WS.acceptRequest pendingConnection
  withClient host $ \backend ->
    concurrently_
      (forever $ WS.receive frontend >>= WS.send backend)
      (forever $ WS.receive backend >>= WS.send frontend)

httpApp :: NetworkId -> FilePath -> Connection -> Application
httpApp networkId keyPath con req send =
  case (requestMethod req, pathInfo req) of
    ("GET", "paint" : args) -> do
      case traverse (readMay . toString) args of
        Just [x, y, red, green, blue] -> do
          putStrLn $ show (x, y) <> " -> " <> show (red, green, blue)
          paintPixel networkId keyPath con Pixel{x, y, red, green, blue}
          send $ responseLBS status200 corsHeaders "OK"
        _ ->
          send handleError
    ("HEAD", _) -> send $ responseLBS status200 corsHeaders ""
    -- Statically serve files
    ("GET", []) -> send $ handleFile "index.html"
    ("GET", ["index.html"]) -> send $ handleFile "index.html"
    ("GET", ["bundle.js"]) -> send $ handleFile "bundle.js"
    ("GET", ["style.css"]) -> send $ handleFile "style.css"
    ("GET", ["logo.png"]) -> send $ handleFile "logo.png"
    _ ->
      send handleNotFound
 where
  handleError = responseLBS status400 corsHeaders "INVALID REQUEST"

  handleNotFound = responseLBS status404 corsHeaders "NOT FOUND"

  handleFile filepath = responseFile status200 corsHeaders filepath Nothing

  corsHeaders =
    [ ("Access-Control-Allow-Origin", "*")
    , ("Access-Control-Allow-Methods", "*")
    , ("Access-Control-Allow-Headers", "*")
    ]
