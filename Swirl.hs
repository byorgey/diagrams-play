{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

swirl :: Int -> Int -> Path R2
swirl k n = mconcat
          . map (cubicSpline True)
          . pathVertices
          . star (StarSkip k)
          $ regPoly n 1

-- d = stroke $ swirl 3 9 <> swirl 3 7 <> swirl 11 23 # scale 0.15
d = (stroke $ swirl 2 5) # fillRule EvenOdd # fc red

main = defaultMain d