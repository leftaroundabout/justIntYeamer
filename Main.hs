{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Presentation.Yeamer.Maths
import Math.LaTeX.StringLiterals
import Text.Cassius
import Data.Semigroup.Numbered

import System.Environment

main :: IO ()
main = yeamer . styling style $ do
   "Just Intonation"
     ====== do
    "What is harmony?"
    "Harmony is time-sequencing of chords."
      ── do
     "What is a chord?"
     "A chord is a stack of thirds."
       ── do
      "What is a third?"
      "Three semitones = minor third." │ "Four semitones = major third."
        ── do
       "What is a semitone?"
       "A semitone is ¹⁄₁₂th of an octave."

style = [cassius|
   body
     height: 100vh
     color: #ccc
     background-color: #000
     font-size: 6vmin
     font-family: "Linux libertine", "Times New Roman"
   .main-title
     font-size: 180%
   h1
     font-size: 150%
   div
     width: 95%
     height: 95%
     text-align: center
     margin: auto
   .headed-container
     height: 80%
   .vertical-concatenation
     display: flex
     flex-direction: column
   .emph
     font-style: italic
   .small
     font-size: 67%
   .verb
     display: inline-block
     font-size: 86%
     background-color: #227
     font-family: "Ubuntu Mono", "Droid Sans mono", "Courier New"

  |] ()
