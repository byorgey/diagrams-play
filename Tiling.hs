{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Tilings

main = defaultMain (drawTiling t4612 10 10)