{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Commons ( module Commons
               , module Presentation.Yeamer
               , module Presentation.Yeamer.Maths
               , module Math.LaTeX.StringLiterals
               , module Text.Hamlet
               , module Data.Semigroup
               , module Data.Semigroup.Numbered
               , module GHC.Exts
               , module Numeric
               , module Data.Ratio

               , module Graphics.Dynamic.Plot.R2

               , module System.Process
               , module Data.Time.Clock
               , module Data.Time.Clock.POSIX
               , module Data.Flat

               , module Control.Monad
               , module Control.Lens
               , module Data.Function

               , module Sound.Tone.Simple ) where

import Presentation.Yeamer
import Presentation.Yeamer.Maths
import Math.LaTeX.StringLiterals
import Text.Hamlet
import Text.Cassius
import Data.Semigroup
import Data.Semigroup.Numbered
import Data.String.Combinators (fromShow)
import GHC.Exts (fromString)
import Numeric (showFFloat)
import Data.Ratio
import Data.Char (toLower)

import Graphics.Dynamic.Plot.R2

import System.Environment
import System.Process (runCommand)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Flat (Flat(..))

import Control.Monad
import Control.Lens hiding ((...), set, (>$))
import Control.Concurrent
import Data.Function (fix)

import Sound.Tone.Simple


dispFreq :: Frequency -> Presentation
dispFreq ν = fromString $ showFFloat (Just 0) ν " Hz"

nameForFreq :: Frequency -> Presentation
nameForFreq ν = decorate $ words
       "C𝅗𝅥 C C𝄽 C♯ D𝅗𝅥 D D𝄽 E♭ E𝅗𝅥 E E𝄽 F F𝄽 F♯ G𝅗𝅥 G G𝄽 G♯ A𝅗𝅥 A A𝄽 B♭ B𝅗𝅥 B B𝄽"
                 !! floor ((relC - fromIntegral octv)*24)
 where relC = logBase 2 (ν / (55*2**(-9.75/12)))
       octv = floor relC
       decorate = (fromString("octave_"++show octv)#%) . fromString . case octv of
          -1 -> (++",")
          0  -> id
          1  -> map toLower
          n  -> (++replicate (n-1) '\'') . map toLower
       
simpleTone :: ToneSpec
simpleTone = ToneSpec 440 1 1 1 1

serveTone :: ToneSpec -> Presentation
serveTone spec = useFileSupplier "wav" (makeTone spec)
   $ \f -> [shamlet| <audio controls loop src=#{f}> |]

style = [cassius|
   body
     height: 100vh
     color: #cde
     background-color: #000
     font-size: 6vmin
     font-family: "Linux libertine", "Times New Roman"
   .main-title
     font-size: 150%
   .thisfreq
     font-weight: bold
   h1
     font-size: 150%
   div
     width: 95%
     height: 95%
     text-align: center
     margin: auto
     border-radius: 30px
     border:10px solid rgba(90,80,40,0.3);
     background: rgba(40,15,15,0.25);
   .octave_-1
     color: black
   .octave_0
     color: red
   .octave_1
     color: orange
   .octave_2
     color: yellow
   .octave_3
     color: green
   .octave_4
     color: cyan
   .octave_5
     color: blue
   .octave_6
     color: violet
   .compact-style div
     font-size: 86%
     width: 98%
     height: 98%
     text-align: center
     margin: auto
     border-radius: 5px
     border:3px solid rgba(90,80,40,0.3);
     background: rgba(40,15,15,0.25);
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
   pre
     text-align: left
     font-size: 86%
     background-color: #204
     font-family: "Ubuntu Mono", "Droid Sans mono", "Courier New"
  |] ()

plotServ :: [DynamicPlottable] -> Presentation -> Presentation
plotServ pl cont = serverSide (forkIO (plotWindow pl >> return ()) >> return ())
                     >> cont



instance Flat UTCTime where
  decode = fmap (posixSecondsToUTCTime . realToFrac . (id::Double->Double)) decode
  encode = encode . (id::Double->Double) . realToFrac . utcTimeToPOSIXSeconds
  size = size . (id::Double->Double) . realToFrac . utcTimeToPOSIXSeconds
