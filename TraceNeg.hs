{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Maybe (fromMaybe)

drawV v = (arrowHead <> shaft) # fc black
  where
    shaft     = origin ~~ (origin .+^ v)
    arrowHead = eqTriangle 0.1
              # rotateBy (direction v - 1/4)
              # translate v
drawTraceV v d
  = lc green $
    fromMaybe mempty
      ((origin ~~) <$> traceP origin v d)
illustrateTraceV v d = (d <> drawV v <> drawTraceV v d) # showOrigin

example = hcat' with {sep = 1}
        . map (illustrateTraceV (0.5 *^ (1 & 1)))
        $ [ circle 1 # translate ((-1.5) & (-1.5))
          , circle 1
          , circle 1 # translate (1.5 & 1.5)
          ]

main = defaultMain example