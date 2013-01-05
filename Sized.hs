{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d = circle 1 ||| circle 2 |||  circle 3

main = defaultMain (d # sized (Dims 3 3))