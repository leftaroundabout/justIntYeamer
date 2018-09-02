{-# LANGUAGE OverloadedStrings #-}

import Presentation.Yeamer

import System.Environment

main :: IO ()
main = yeamer $ do
   "Just Intonation"
     ====== do
    "What is harmony?"
