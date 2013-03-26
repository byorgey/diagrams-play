{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

f x = 3 - (x - 2)^2

para lo hi = cubicSpline False [(x & f x) | x <- [lo, lo + 0.01 .. hi ]]

dia = para 0 3 <> hrule 3 <> circle 0.1 # translateX (2 - sqrt 3) <> region # fc lightblue # lw 0 # translateX 3

main = defaultMain (dia # centerXY # pad 1.1)

z1 = 2 - sqrt 3

region = strokeT (hrule (3 - z1) # reflectX <> para z1 3 <> vrule (f 3))