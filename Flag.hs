{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude hiding (union)
import Diagrams.Backend.Cairo.CmdLine
import Graphics.Rendering.Diagrams.Points

stripe c = rect 1.9 (1/13) # lw 0 # fc c

stripes = (vcat . take 13 . map stripe $ cycle [red, white]) # alignTL

union = (stars <> field) # alignTL

uLen = (2/5 * 1.9)
uHt  = 7/13

field = rect uLen uHt # lw 0 # fc blue

stars = centerXY
      . vcat' with {catMethod = Distrib, sep = uHt/10}
      . take 9 . map starRow $ cycle [6,5]

starRow n = centerX
          . hcat' with {catMethod = Distrib, sep = uLen/6}
          $ replicate n astar

astar = (stroke $ star (StarSkip 2) (regPoly 5 (0.0616/2)))
      # lw 0
      # fc white

flag = (union <> stripes) # centerXY

main = defaultMain (pad 1.1 flag)
-- main = defaultMain stars