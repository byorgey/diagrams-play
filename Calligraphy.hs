{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Coordinates
import           Diagrams.Prelude
import           Linear.Vector                  (negated)

callig :: V2 Double -> Trail' Line V2 Double -> Trail' Loop V2 Double
callig pen p
  = (p <> fromOffsets [pen] <> reverseLine p <> fromOffsets [negated pen])
  # closeLine

o :: Trail' Loop V2 Double
o = callig (2 ^& 1) (circle 15)

curve = cubicSpline False [1 ^& 1, 2 ^& 5, 5 ^& 6, 8 ^& 12]

-- main = defaultMain (callig (0.5 ^& 0.5) curve # strokeLoop # fc black)
main = defaultMain (strokeLoop o # fc black)
