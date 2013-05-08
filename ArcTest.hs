{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.List.Split
import           Data.Maybe
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

butterfly =
  lw 0 .
  mconcat $
  zipWith fcA colors
  [ arcBetween (ArcRadius r False) origin p # fromMaybe mempty
  | p <- [ (-3)&2, 3&2, (-2)&(-3), 2&(-3) ]
  , r <- [3.5, 3.3 .. 0] ++ [-3.5, -3.3 .. 0]
  ]

colors = map (`withOpacity` 0.8) $ cycle [red, orange, yellow, green, blue, purple]

{-
a = mconcat [ fromMaybe mempty $ arcBetween r p1 (3 & 1)
            | r <- [-3, -2.8 .. 3]
            , p1 <- [1&0, 2&0, 3&0, origin]
            ]
-}

a = mconcat [fromJust $ arcBetween (ArcOffset off) origin (1&1) | off <- [1, 0.9 .. 0]]

main = defaultMain (a # centerXY # pad 1.1)

-- grid = centerXY . vcat . map hcat . chunksOf 10 . replicate 100 $ square 1
