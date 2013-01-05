{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

sq c = square 1 # fc c # lw 0

row n c1 c2 = map sq (cycle [c1,c2])
            # take n
            # hcat

board n c1 c2 = centerXY
              . vcat 
              . take n . map (uncurry (row n)) 
              $ cycle [(c1,c2), (c2,c1)]

border n = (halfBorder <> rotateBy (1/2) halfBorder)
         # close
         # strokeT # centerXY # lw 0.05
         # scale 1.01
  where halfBorder = fromOffsets [unit_X, (n-1) *^ unit_Y, (n-1) *^ unitX, unitY]

dia = border 8 <> board 8 white black

main = defaultMain (dia # pad 1.1)