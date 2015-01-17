{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

main = defaultMain ((square 20 ||| circle 10) # lw medium # frame 1)
