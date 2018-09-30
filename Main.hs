{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Commons

import Presentation.Yeamer
import Presentation.Yeamer.Maths
import Math.LaTeX.StringLiterals
import Text.Hamlet
import Data.Semigroup
import Data.Semigroup.Numbered
import GHC.Exts (fromString)
import Numeric (showFFloat)
import Data.Ratio

import Graphics.Dynamic.Plot.R2

import System.Process (runCommand)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Flat (Flat(..))

import Control.Monad
import Control.Lens
import Data.Function (fix)

import Sound.Tone.Simple

main :: IO ()
main = yeamer . styling style $ do
   ""──
     "main-title"#%
       "(Ab)using a monadic screen-presentation eDSL as a just-intonation synth pad controller"
     ──
     "Justus Sagemüller"
     ──
     "reference"#%("Institut für Geophysik und Meteorologie"──"Universität zu Köln")
   
   "Just Intonation"
     ====== do
    "What is harmony?"
    "Harmony is time-sequencing of chords."
      ── do
     "What is a chord?"
     "A chord is a stack of thirds."
       ── do
      "What is a third?"
      serverSide $ runCommand "pasuspender pianoteq" >> return ()
      "Minor third = three semitones." │ "Major third = four semitones."
        ── do
       "What is a semitone?"
       "A semitone is ¹⁄₁₂th of an octave."
      plotServ [ plotLatest [ continFnPlot ((+y₀) . f . (+t₀))
                                  & plotDelay (1/40)
                            | t₀ <- [0,1/20..] ]
                  & legendName capt
               | let wavefm t'
                       | t < 1/3    = 3*(t*3)^2 - 2*(t*3)^3
                       | otherwise  = let tA = (1 - t)/2
                                      in 3*(tA*3)^2 - 2*(tA*3)^3
                      where t = t' - fromIntegral (floor t' :: Int)
                     sigs = [wavefm . (+sin (f*30)) . (*f) | f<-[4,5,6]]
               , (y₀,capt,f)
                   <- zip3 [0,-2..] ["4 Hz", "5 Hz", "6 Hz", "∑"]
                        $ sigs ++ [foldr (\f g x -> f x+g x) (const 0) sigs]
               ]
          $ "Minor third = frequency ratio 6:5." │ "Major third = frequency ratio 5:4."
     "A chord is a compound of notes whose pitches are at a small integral ratio."
   
   "Temperaments"
     ====== do
      "Approximations of 5-limit rationals by logarithmically regular grid (12-edo):"
       <> maths [ [ fromIntegral (numerator r)/fromIntegral (denominator r)
                         ⩵ fromString (showFFloat (Just 3) (fromRational r :: Double) "")
                  , fromString (showFFloat (Just 3) (2**(fromIntegral n/12) :: Double) "")
                         ⩵ 2◝(fromIntegral n/12)
                       :: Math ]
                | (r,n) <- [ (3/2, 7)
                           , (5/4, 4)
                           , (6/5, 3) ] ]""
      "Approximations of 7-limit rationals by logarithmically regular grid (31-edo):"
       <> maths [ [ fromIntegral (numerator r)/fromIntegral (denominator r)
                         ⩵ fromString (showFFloat (Just 3) (fromRational r :: Double) "")
                  , fromString (showFFloat (Just 3) (2**(fromIntegral n/31) :: Double) "")
                         ⩵ 2◝(fromIntegral n/31)
                       :: Math ]
                | (r,n) <- [ (3/2, 18)
                           , (5/4, 10)
                           , (7/4, 25)
                           , (11/9, 9) ] ]""

   "Beamonad"
     ====== do
    ( "Juxtapose" │ "content"
                 ──
        "like"    │  "this"   )
      ──[plaintext|
      "Juxtapose" │ "content"
                 ──
        "like"    │  "this"   |]
    ( "Juxtapose" ||| "content"
                  ===
        "like"    |||  "this" )
      ──do
     [plaintext|
      "Juxtapose" ||| "content"
                  ===
        "like"    |||  "this" |]
     [plaintext|
    (("Juxtapose" ||| "content")
                  ===
        "like")   |||  "this" |]
     [plaintext|
    (("Juxtapose" ||| "content")
                  ===
                "like"          )   |||  "this" |]
    (do
     "Time-sequence content"
     "like this")
      ──[plaintext|
          do
           "Time-sequence content"
           "like this" |]
   
   forM_ [ \f -> [shamlet| <audio controls      src=#{f}> |]
         , \f -> [shamlet| <audio controls loop src=#{f}> |] ] $ \audioUsage ->
     "A simple tetrachord of notes"
      ====== do
       let ν₀ = 110
       foldr1 (──)
        [ (dispFreq ν
              <>" ( = "<>(fromIntegral (numerator rat)
                            /fromIntegral (denominator rat) × 110⁀"Hz" :: Math) $<>")")
         <> useFileSupplier "wav" (makeTone $ simpleTone & frequency .~ ν) audioUsage
        | rat <- [1, 2, 9/4, 5/2, 8/3]
        , let ν = fromRational rat * ν₀ ]
    
   "Hydra"
    ======
     fix (\h -> "head" >>= \() -> h │ h
                                   ──
                                  h │ h )
    ── do [plaintext|
           fix (\h -> "head" >>= \() -> h │ h
                                         ──
                                        h │ h ) |]
          [plaintext|
           fix (\h -> "head" >> h │ h
                                 ──
                                h │ h ) |]
    
   forM_ [ (id, dispFreq, 55)
         , (("compact-style"#%), dispFreq, 55)
         , (("compact-style"#%), nameForFreq, 55*2**(-1/3)) ]
      $ \(mdf, labelling, ν₀) -> mdf $
    "The tree of 5-limit notes"
    ====== do
     let node ν = do
          () <- labelling ν<>"..."
          node (ν*2)
           ── ("thisfreq"#%labelling ν<>":"<>serveTone (simpleTone & frequency .~ ν))
           ── node (ν*5/4) │ node (ν*3/2)
     node ν₀
    
   forM_ [ (("compact-style"#%), nameForFreq, 110*2**(-1/3)) ]
      $ \(mdf, labelling, ν₀) -> mdf $
    "The tree of 7-limit notes"
    ====== do
     let node ν = do
          () <- labelling ν<>"..."
          node (ν*2) │ node (ν*7/4)
           ── ("thisfreq"#%labelling ν<>":"<>serveTone (simpleTone & frequency .~ ν))
           ── node (ν*5/4) │ node (ν*3/2)
     node ν₀
    
   forM_ [ (("compact-style"#%), nameForFreq, 55*2**(-1/3)) ]
      $ \(mdf, labelling, ν₀) -> mdf $
    "The tree of odd-11-limit notes"
    ====== do
     let node ν = do
          () <- labelling ν<>"..."
          node (ν*3) │ node (ν*11/3)
           ── ("thisfreq"#%labelling ν<>":"<>serveTone
                             (simpleTone & frequency .~ ν
                                         & loudness .~ 2
                                         & evenHarmonicsContent .~ 0))
           ── node (ν*5/3) │ node (ν*7/3)
     node ν₀
   
   "Using the monad capability for time control"
    ====== (do
     "Want to know the time?"
     t <- serverSide getCurrentTime
     fromString $ show t)
    ── [plaintext|
        do
          "Want to know the time?"
          t <- serverSide getCurrentTime
          fromString $ show t |]

   forM_ [ (("compact-style"#%), nameForFreq, 55*2**(-1/3)) ]
      $ \(mdf, labelling, ν₀) -> mdf $
    "The tree of 5-limit notes"
    ====== do
     let node ν = do
          () <- labelling ν<>"..."
          ttIn <- serverSide getCurrentTime
          () <- labelling ν<>"..?"
          ttOut <- serverSide getCurrentTime
          let tNote = fromIntegral (round $ diffUTCTime ttOut ttIn * 2) / 2
          node (ν*2)
           ── ("thisfreq"#%(labelling ν)<>":"<>serveTone (simpleTone
                                                   & duration .~ tNote
                                                   & frequency .~ ν ))
           ── node (ν*5/4) │ node (ν*3/2)
     node ν₀
    

