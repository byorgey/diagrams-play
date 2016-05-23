{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

p :: Diagram B
p = (stroke $ star (StarSkip 2) (reflectY (regPoly 5 1))) # lc red

main = defaultMain p
