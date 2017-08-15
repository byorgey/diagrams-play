{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

dia = mconcat
  [ arrowV' (with & shaftStyle %~ dashingL [5, 5] 0) unitX
  , (origin ~~ (1 ^& 1)) # dashingL [0.1, 0.1] 0
  , square 2
  ]

main = defaultMain (dia # frame 1)
