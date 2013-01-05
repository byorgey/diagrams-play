{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

info = [ ((2   , 5  ), "shoes"      )
       , ((1   , 6  ), "ships"      )
       , ((1.5 , 4  ), "sealing wax")
       , ((1   , 4.5), "cabbages"   )
       , ((1.3 , 7  ), "kings"      )
       ]

bars = map mkBar info
  where
    mkBar ((x,y), label) =
      rect x y
        # alignB
      ===
      alignedText 1 0.5 label
        # rotateBy (1/4)

dia = hcat bars # fc blue # centerXY # pad 1.1

main = defaultMain dia