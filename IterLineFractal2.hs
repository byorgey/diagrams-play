{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List.Split
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Path.IteratedSubset

main = defaultMain $
  showGenerator terDragonGen

-- main = defaultMain $
--   iterGenerator invTerDragonGen
--   # take 8
--   # map (bevelLine #> strokeLine #> lw veryThin #> sized (dims (1 ^& 1)))
--   # chunksOf 4
--   # map (hsep 0.2)
--   # vsep 0.2
--   # frame 0.2

(#>) = flip (.)
