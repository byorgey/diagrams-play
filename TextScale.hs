{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

t = text "F" <> square 1
s = text "G" <> square 1

main = defaultMain (t # scale 2 ||| s # scale (1/2))