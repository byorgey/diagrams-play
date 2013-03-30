{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           ShrinkPath

hexaflower :: Int -> Double -> [Path R2]
hexaflower n s = iterateN n (shrinkPath s) (hexagon 1)

dia = hexaflower 50 0.8
    # map stroke
    # lw 0
    # zipWith fc (cycle [yellow, orange, red, purple, blue, green])
    # reverse
    # mconcat
    # sized (Width 2)

main = defaultMain dia
