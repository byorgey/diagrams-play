-- Based on http://www.jasondavies.com/primos/ and http://www.polprimos.com/ .

{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Active
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.SRGB
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Math.NumberTheory.Primes       (isPrime)

squiggle len c d
  = cycle [ semi, semi # reflectY ]
  # take wholeArcs
  # (++ [partialArc])
  # hcat
  # alignL
  # lc c
--  # lcA (hsvBlend (d / len) (blend 0.9 white blue) blue `withOpacity` 0.2)
  where
    r          = d/2
    semi       = arc 0 (tau/2 :: Rad) # scale r
    wholeArcs  = floor (len / d)
    remLen     = len - fromIntegral wholeArcs*d
    theta      = acos ((remLen - r) / r)
    partialArc = arc (Rad theta) (tau/2) # scale r
               # if even wholeArcs then id else reflectY

colors :: [Colour Double]
colors = map (uncurryRGB sRGB . fromHSV) (iterate ((`fmod` 360) . (+ga)) 0)
  where
    ga = 180 * (3 - sqrt 5)
    fromHSV h  = hsv h 1 1
    x `fmod` n | x >= n     = x - n
               | otherwise = x

squiggles len = (zipWith (squiggle len) colors . map fromIntegral . filter isPrime $ [2 .. floor len - 1])
              # mconcat
              # beneath dots
              # withEnvelope (mempty :: D R2)
--              # atop (strutX (len + 1) # alignL)
              # atop (strutX 180 # alignL)
  where
--    dots = replicate (ceiling len + 2) (circle 0.2 # fc white)
    dots = replicate 181 (circle 0.2 # fc white # lc white)
         # hcat' with { catMethod = Distrib, sep = 1 }

hsvBlend t c1 c2 = uncurryRGB sRGB . hsv3 $ lerp h2 h1 t
  where
    [h1, h2] = map (hsvView . toSRGB) [c1,c2]
    fst3 (x,_,_) = x
    hsv3 (h,s,v) = hsv h s v

{-
main = defaultMain (squiggles l # sized (Width 10)
                                # beneath (square (l+1) # fc black # alignL)
                   )
  where l = 7
-}

main = animMain (mkActive 0 60 (\t -> squiggles (fromTime t * 3) # sized (Width 10)))
