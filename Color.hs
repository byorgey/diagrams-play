{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

colors = map (\k -> blend k blue yellow) [0, 0.05 .. 1]

main = defaultMain (hcat . map (\c -> square 1 # fc c # lw 0) $ colors)
