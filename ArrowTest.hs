{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

c1 = circle 1 # named "c1"
c2 = circle 1 # named "c2"

dia = c1 # translateY 1 <> c2 # translateX 2

main = defaultMain (dia # connect "c1" "c2")
