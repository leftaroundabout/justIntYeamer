-- |
-- Module      : Sound.Tone.Simple
-- Copyright   : (c) Justus Sagemüller 2018
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE TemplateHaskell     #-}

module Sound.Tone.Simple ( Frequency, Duration, Amplitude
                         , ToneSpec(ToneSpec), frequency, duration, loudness, decayTime
                         , makeTone ) where
    
import qualified Sound.File.Sndfile as HSnd
import qualified Sound.File.Sndfile.Buffer.StorableVector as HSnd
import qualified Data.StorableVector as UArr

import Data.Int
import Control.Arrow
import Control.Lens

type Frequency = Double  -- In Hertz
type Duration = Double   -- In seconds
type Amplitude = Double  -- 1 = full scale

data ToneSpec = ToneSpec
      { _frequency :: !Frequency
      , _duration :: !Duration
      , _loudness :: !Amplitude
      , _decayTime :: !Duration
      }
makeLenses ''ToneSpec

νSample :: Frequency
νSample = 44100

overdrive :: Amplitude -> Amplitude
overdrive p = p / (1 + p^2)

makeTone :: ToneSpec -> FilePath -> IO ()
makeTone spec tgt = fmap mempty . HSnd.writeFile info tgt
          . HSnd.toBuffer . UArr.sample nSpl
         $ fromIntegral >>> \i
             -> let φ = ω'Spl*i
                    t = tSpl*i
                 in round $ fromIntegral (maxBound :: Int16)
                            * overdrive (ampl * (sin φ + sin (2*φ)) * exp (-6*t))
                      :: Int16
 where info = HSnd.Info nSpl (round νSample) 1 sndFormat 1 True
       nSpl = round $ spec^.duration * νSample
       ω'Spl = 2*pi*spec^.frequency/νSample
       tSpl = 1/νSample
       ampl = spec^.loudness * 10 / sqrt (spec^.frequency)

sndFormat :: HSnd.Format
sndFormat = HSnd.Format {
    HSnd.headerFormat = HSnd.HeaderFormatWav
  , HSnd.sampleFormat = HSnd.SampleFormatPcm16
  , HSnd.endianFormat = HSnd.EndianFile
  }
