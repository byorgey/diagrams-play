{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Maybe
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Path.IteratedSubset

flowsnakeRoot = fromOffsets . map toOff $ [0, 5, 3, 4, 0, 0, 1]
  where toOff k = unitY # rotateBy (k/6)

flowsnake 0 = flowsnakeRoot
flowsnake n = mconcat . catMaybes . zipWith refineSegment fs . trailSegments $ flowsnakeRoot
  where
    f  = flowsnake (n-1)
    fs = map ($f) [id, reverseTrail, reverseTrail, id, id, id, reverseTrail]

dia = strokeT $ flowsnake 3

main = defaultMain (dia # centerXY # pad 1.1)


