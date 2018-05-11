{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import Data.Colour.Palette.Types (Kolor)
import Data.Colour.Palette.RandomColor

field :: Int -> [Kolor] -> Diagram B
field n pal = vcat' (with & catMethod .~ Distrib & sep .~ 1)
    $ map (hcat' (with & catMethod .~ Distrib & sep .~ 1))
    $ lw thin
    $ [ [ hrule 1
          # rotateBy (fromIntegral (20*r + c^2) / (n' + n'^2))
          # lc (pal !! (((r - c) `div` 4) `mod` (length pal)))
        | c <- [0..n]
        ]
      | r <- [0..n]
      ]
    where
      n' = fromIntegral n

main = do
  pal <- randomCIELabPalette
  defaultMain $ field 30 pal # frame 1
