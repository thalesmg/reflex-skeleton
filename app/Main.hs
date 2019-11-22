{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex
import Reflex.Dom

main = mainWidget $ el "div" $ do
  t <- textArea def
  el "div" $
    dynText $ _textArea_value t
