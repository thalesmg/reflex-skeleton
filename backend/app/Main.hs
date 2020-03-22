{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Web.Scotty

main :: IO ()
main =
  scotty 4040 $ do
    get "/" $ file "./index.html"
    get "/api" $ json ("Hello from the backend!" :: Text)
    get "/:file" $ do
      staticFile <- param "file"
      file staticFile
