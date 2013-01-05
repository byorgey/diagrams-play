{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

cp = circle 1 # translateX 2

c = cp # fc blue

n = 10

d = mconcat . tail . iterateN n (rotateBy $ 1/n) $ c

c' = c # rotateBy (-1/10) # clipBy cp

main = defaultMain (pad 1.1 (c' <> c <> d))