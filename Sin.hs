{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmF she #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

x = (| cos (interval 0 (toTime tau)) |)
y = (| sin (interval 0 (toTime tau)) |)

pt = circle 0.3 # fc black

a1 = (| translateX x (| translateY y ~pt |) |)
a2 = (| ~origin ~~ (| mkPt x y |) |)  # lw 0.05 # lineCap LineCapRound
  where mkPt x y = P (x,y)

a = trim $ a1 <> a2

main = animMain ((a # lc blue ->> backwards a # lc green) <> square 3 # fc white )