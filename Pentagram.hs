{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

p = (stroke $ star (StarSkip 2) (reflectY (regPoly 5 1))) # lc red

main = defaultMain p