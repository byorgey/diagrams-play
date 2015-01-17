{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

main = defaultMain ((square 1 <> phantom (circle 1 :: D R2)) ||| square 1)
