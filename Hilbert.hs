{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

hilbert 0 = mempty
hilbert n = half <> hrule 1 <> half # reflectX # reverseTrail
  where
    half = h' # rotateBy (-1/4) # reverseTrail <> vrule (-1) <> h'
    h' = hilbert (n-1)

diagram :: Diagram B
diagram = strokeT $ hilbert 5

Main = mainWith (diagram # frame 0.1)
