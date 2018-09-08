-- |
-- Module      : Sound.Tone.Simple
-- Copyright   : (c) Justus Sagemüller 2018
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Sound.Tone.Simple (Frequency, Duration, makeTone) where
    
import qualified Sound.File.Sndfile as HSnd
import qualified Sound.File.Sndfile.Buffer.StorableVector as HSnd
import qualified Data.StorableVector as UArr

import Data.Int

type Frequency = Double  -- In Hertz
type Duration = Double   -- In seconds

νSample :: Frequency
νSample = 44100

makeTone :: Duration -> Frequency -> FilePath -> IO ()
makeTone t ν tgt = fmap mempty . HSnd.writeFile info tgt
          . HSnd.toBuffer . UArr.sample nSpl
         $ \i -> let φ = ω'Spl*fromIntegral i
                 in round $ fromIntegral (maxBound :: Int16) * sin φ :: Int16
 where info = HSnd.Info nSpl (round νSample) 1 sndFormat 1 True
       nSpl = round $ t * νSample
       ω'Spl = 2*pi*ν/νSample

sndFormat :: HSnd.Format
sndFormat = HSnd.Format {
    HSnd.headerFormat = HSnd.HeaderFormatWav
  , HSnd.sampleFormat = HSnd.SampleFormatPcm16
  , HSnd.endianFormat = HSnd.EndianFile
  }
