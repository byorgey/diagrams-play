{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

main = defaultMain (let ls = hcat' with {catMethod = Distrib, sep = 1} (replicate 11 (vrule 10)) # centerXY in ls <> rotateBy (1/4) ls)
