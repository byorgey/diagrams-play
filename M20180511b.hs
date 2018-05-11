{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import Data.Colour.Palette.Types (Kolor)
import Data.Colour.Palette.RandomColor

import qualified Numeric.Noise.Perlin     as P

field :: Int -> Int -> [Kolor] -> Diagram B
field seed n pal =
  [ [ square side
      # fillTexture rg
      # opacity 0.4
      # rotate (perlin2d 2 r' c' / 20 @@ turn)
    | c <- [0..n]
    , let c' = fromIntegral c
    , let side = perlin2d 0 r' c' + 1.2
    , let col = interpolatePalette (perlin2d 1 r' c') pal
    , let rg = mkRadialGradient (mkStops [(col,0,0.4), (white,1,0)])
                 origin (side / 2 - 0.1) origin (side / 2 * sqrt 2)
                 GradPad
    ]
  | r <- [0..n], let r' = fromIntegral r
  ]
  # lw none
  # map (hcat' (with & catMethod .~ Distrib & sep .~ 1))
  # vcat' (with & catMethod .~ Distrib & sep .~ 1)
  where
    n' = fromIntegral n

    -- From https://www.kovach.me/posts/2018-03-07-generating-art.html
    perlinOctaves = 5
    perlinScale = 0.1
    perlinPersistance = 0.5
    perlinNoise
      = P.perlin seed perlinOctaves perlinScale perlinPersistance

    perlin2d i x y
      = P.noiseValue perlinNoise (x + seed', y + seed', seed')
      where
        seed' = fromIntegral (iterate next seed !! i)
        next k = (1664525 * k + 1013904223) `mod` (2^32)

interpolatePalette i pal = blend (i - fromIntegral (floor i)) c1 c0
  where
    n  = length pal
    c0 = pal !! (floor i `mod` n)
    c1 = pal !! (ceiling i `mod` n)

main = do
  pal <- randomCIELabPalette
  defaultMain $ field 1 30 pal # frame 1
