{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

callig :: R2 -> Trail R2 -> Trail R2
callig offs p = (p <> fromOffsets [offs] <> reverseTrail p <> fromOffsets [negateV offs]) # close

o :: Trail R2
o = callig (2 & 1) (circle 15)

curve = cubicSpline False [1 & 1, 2 & 5, 5 & 6, 8 & 12]

main = defaultMain (callig (0.5 & 0.5) curve # strokeT # fillRule EvenOdd # fc black)
