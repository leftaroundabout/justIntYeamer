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
    
   forM_ [id, ("compact-style"#%)] $ \mdf -> mdf $
    "The tree of 5-limit notes"
    ====== do
     let node ν = do
          () <- dispFreq ν
          node (ν*2)
           ── ("thisfreq"#%dispFreq ν<>":"<>serveTone (simpleTone & frequency .~ ν))
           ── node (ν*5/4) │ node (ν*3/2)
     node 55

dispFreq :: Frequency -> Presentation
dispFreq ν = fromString $ showFFloat (Just 0) ν " Hz"
       
simpleTone :: ToneSpec
simpleTone = ToneSpec 440 1 1 1

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

