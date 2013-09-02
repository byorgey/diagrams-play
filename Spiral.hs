
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

points = zipWith (\r th -> origin .+^ (r *^ e th))
           [1, 1.25 .. ]
           [0 :: Turn, 1/4 .. ]

dia = cubicSpline False (take 20 points)
  -- mconcat . map (place dot) . take 20 $ points

dot = circle 0.2 # fc green

main = defaultMain  dia
