-- |
-- Module      : Sound.Tone.Simple
-- Copyright   : (c) Justus Sagemüller 2018
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Sound.Tone.Simple where
    
import Sound.File.Sndfile.Buffer.StorableVector
import qualified Data.Vector.Storable as UArr

type Frequency = Double  -- In Hertz
type Duration = Double   -- In seconds

makeTone :: Duration -> Frequency -> IO FilePath
makeTone t ν = undefined
