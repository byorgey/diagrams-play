{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Control.Lens

dia = mconcat . (element 2 %~ lc blue) . concat . explodePath $ square 1

main = defaultMain (dia # pad 1.1)