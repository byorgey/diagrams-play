{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Coordinates
import           Diagrams.Prelude

main = defaultMain $ mconcat
  [ arcBetween (p & 0) (q & 1) ht | p <- [0..2], q <- [0..2], ht <- [-0.2, -0.1 .. 0.2]]
