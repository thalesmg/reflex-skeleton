{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (someValue)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
  ePost <- getPostBuild
  result <- fmap (fromMaybe ("failed to call backend!" :: Text)) <$> getAndDecode ("/api" <$ ePost)
  fromBackend <- holdDyn "fetching data from backend..." result
  t <- textAreaElement def
  el "p" $ text "Hello, skeleton!"
  el "div" $
    dynText $ _textAreaElement_value t
  el "p" $ text someValue
  el "p" $ dynText fromBackend
