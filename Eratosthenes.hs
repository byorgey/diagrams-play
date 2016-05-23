{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import           Math.NumberTheory.Primes            (primes)

data Wheel = Wheel { wheelRad :: Int, wheelDist :: Double }

mkWheel :: Int -> Wheel
mkWheel n = Wheel n (fromIntegral n)

roll :: Double -> Wheel -> Wheel
roll dd (Wheel n d) = Wheel n (d + dd)

drawWheel :: Wheel -> Diagram B
drawWheel (Wheel n d)
  = wheelShape n
  # rotate (-d * tau / fromIntegral n @@ rad)
  # translateX (d * tau)
  # opacity (min 1 ((d - fromIntegral n) * 2))

wheelShape :: Int -> Diagram B
wheelShape n = strokeP $ mconcat
  [ circle n'
  , wedge (n' - 0.5)
          (xDir # rotate (pi/n' - pi/2 @@ rad))
          ((n'-1) * tau / n' @@ rad)
    # reversePath
  ]
  where
    n' = fromIntegral n

olddia = mconcat (map (alignB . drawWheel . roll 1 . mkWheel) [2 .. 8]) # fc blue

pdia :: Double -> Diagram B
pdia d
  = mconcat
  . map (alignB . fc blue . drawWheel . theWheel)
  . takeWhile (<= floor d)
  . map fromInteger
  $ primes
  where
    theWheel p = mkWheel p # roll (d - fromIntegral p)

numberline :: Int -> Int -> Diagram B
numberline a b = hcat' (with & catMethod .~ Distrib & sep .~ tau)
  (map (text . show) [a .. b])
  # translateX (fromIntegral a*tau)
  # translateY 1.3
  # extrudeLeft tau # extrudeRight tau

mkFrame d = pdia d <> numberline 2 10

main = gifMain [(mkFrame d, 3) | d <- [2, 2 + 1/30 .. 10] ]
