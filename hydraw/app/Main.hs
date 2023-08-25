module Main where

import Hydra.Prelude

import Network.HTTP.Types.Status (status200, status404)
import Network.Wai (
  Application,
  pathInfo,
  requestMethod,
  responseFile,
  responseLBS,
 )
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  Warp.runSettings settings httpApp
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

httpApp :: Application
httpApp req send =
  case (requestMethod req, pathInfo req) of
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
  handleNotFound = responseLBS status404 corsHeaders "NOT FOUND"

  handleFile filepath = responseFile status200 corsHeaders filepath Nothing

  corsHeaders =
    [ ("Access-Control-Allow-Origin", "*")
    , ("Access-Control-Allow-Methods", "*")
    , ("Access-Control-Allow-Headers", "*")
    ]
