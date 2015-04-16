{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

main = defaultMain $ dia # frame 0.1

dia = arrowheads
   -- vrule 1 ||| arrows ||| vrule 1

arrowheads =
  mconcat
  [ drawHead halfDart # translateY 0.2
  , drawHead dart
  ]

arrows = vcat' (with & catMethod .~ Distrib & sep .~ 0.05)
         [ arrowV' (with & arrowHead .~ halfDart) unitX
         , arrowV unitX
         ]

drawHead hd
  = stroke h <> stroke j
  where
      (h,j) = hd 0.01 0.5
