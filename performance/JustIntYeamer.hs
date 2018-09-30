{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Commons


main :: IO ()
main = yeamer . styling style $ do
   ""──
     "main-title"#%
       "(Ab)using a monadic screen-presentation eDSL as a just-intonation synth pad controller"
     ──
     "Justus Sagemüller"
     ──
     "reference"#%("Institut für Geophysik und Meteorologie"──"Universität zu Köln")
   
   forM_ [ (("compact-style"#%), nameForFreq, 55*2**(-1/3)) ]
      $ \(mdf, labelling, ν₀) -> mdf $
    "The tree of 7-limit notes"
    ====== do
     let node ν = do
          () <- labelling ν<>"..."
          ttIn <- serverSide getCurrentTime
          () <- labelling ν<>"..?"
          ttOut <- serverSide getCurrentTime
          let tNote = fromIntegral (round $ diffUTCTime ttOut ttIn * 2) / 2
          node (ν*2) │ node (ν*7/4)
           ── ("thisfreq"#%(labelling ν)<>":"<>serveTone (simpleTone
                                                   & duration .~ tNote
                                                   & frequency .~ ν ))
           ── node (ν*5/4) │ node (ν*3/2)
     node ν₀
    

