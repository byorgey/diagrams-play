{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain $ (circle 10 ||| circle 30 ||| circle 50 # fc red # lw 5) # lw 1