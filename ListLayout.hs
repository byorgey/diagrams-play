{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

t = eqTriangle 5   # fc orange
s = square     3   # fc red
o = ellipseXY  2 3 # fc blue
c = circle     2   # fc green

d = centerX (t ||| s ||| o ||| c)

ds = centerX ([t] ||| [s] ||| [o] ||| [c])

ds' = mconcat $ zipWith translateY [0.2, -0.3, 0, 0.1] ds

main = defaultMain ds'