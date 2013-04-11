{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           ShrinkPath

dia = (take 50 $ iterate (shrinkPath 0.9) (hexagon 1))
    # mconcat # stroke # lc green

main = defaultMain (dia # centerXY # pad 1.1)
