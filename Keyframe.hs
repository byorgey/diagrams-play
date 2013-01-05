{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Maybe

c = circle 2 # fc red
t = eqTriangle 3 # fc green
s = square 5 # fc orange
o = ellipseXY 2 3 # fc purple

main = defaultMain (mconcat . everyOther $ [c] ||| [t] ||| [s] ||| [o])

everyOther = catMaybes . zipWith ($) (cycle [const Nothing, Just])