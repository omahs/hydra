module Main where

import Hydra.Prelude

import Hydra.Cardano.Api (AsType (..))
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Painter (Pixel (..), mkPaintTx)
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.Wai (
  Application,
  pathInfo,
  requestMethod,
  responseFile,
  responseLBS,
 )
import qualified Network.Wai.Handler.Warp as Warp
import Safe (readMay)
import Test.QuickCheck (generate)

main :: IO ()
main = do
  key <- requireEnv "HYDRAW_CARDANO_SIGNING_KEY"
  Warp.runSettings settings (httpApp key)
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

-- | Like 'lookupEnv' but terminate program with a message if environment
-- variable is not set.
requireEnv :: String -> IO String
requireEnv name =
  lookupEnv name >>= \case
    Just value -> pure value
    Nothing -> die $ "Error: Required environment variable " <> name <> " not set"

httpApp :: FilePath -> Application
httpApp keyPath req send =
  case (requestMethod req, pathInfo req) of
    ("GET", "paint" : args) -> do
      case traverse (readMay . toString) args of
        Just [x, y, red, green, blue] -> do
          putStrLn $ show (x, y) <> " -> " <> show (red, green, blue)

          sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) keyPath
          (txIn, txOut) <- generate arbitrary
          case mkPaintTx (txIn, txOut) sk Pixel{x, y, red, green, blue} of
            Right tx -> do
              putStrLn (renderTx tx)
              send $ responseLBS status200 corsHeaders "OK"
            Left err -> fail $ "error: " <> show err
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
