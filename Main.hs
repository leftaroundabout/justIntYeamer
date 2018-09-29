{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

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
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.Backend.Cairo as Dia

import System.Environment
import Control.Monad
import Control.Lens
import Control.Concurrent
import Data.Function (fix)

import Sound.Tone.Simple

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
     font-size: 180%
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

plotStat :: ViewportConfig -> [DynamicPlottable] -> Presentation
plotStat viewCfg pl = imageFromFileSupplier "png" $ \file -> do
    prerendered <- plotPrerender viewCfg pl
    Dia.renderCairo file
                    (Dia.mkSizeSpec $ Just (fromIntegral $ viewCfg^.xResV)
                               Dia.^& Just (fromIntegral $ viewCfg^.yResV))
                    prerendered

