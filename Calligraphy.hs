{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Coordinates
import           Diagrams.Prelude

callig :: R2 -> Trail' Line R2 -> Trail' Loop R2
callig pen p
  = (p <> fromOffsets [pen] <> reverseLine p <> fromOffsets [negateV pen])
  # closeLine

o :: Trail' Loop R2
o = callig (2 & 1) (circle 15)

curve = cubicSpline False [1 & 1, 2 & 5, 5 & 6, 8 & 12]

-- main = defaultMain (callig (0.5 & 0.5) curve # strokeLoop # fc black)
main = defaultMain (strokeLoop o # fc black)
