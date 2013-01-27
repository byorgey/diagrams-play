{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

c = circle 1 # fc red

dia = vcat (replicate 3 c)

p = fromOffsets [(1 & 2), (-3 & 4), (0 & 3)]

q = mconcat . take 10 . iterate (rotateBy (1/10)) $ p

myStar = strokeT q # lw 0.1 # centerXY

stars = mconcat . reverse . zipWith fc colors . take 10 . iterate (rotateBy (1/40) . scale 0.8) $ myStar

colors = cycle [red, blue, green, yellow]

main = defaultMain stars