{-# LANGUAGE NoMonomorphismRestriction #-}

-- Based on http://lostinrecursion.wordpress.com/2013/03/16/stars-of-the-minds-sky/

import           Control.Arrow                  ((***))
import           Data.Colour.SRGB
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

orbits :: Int -> (Diagram Cairo R2, Diagram Cairo R2)
orbits n = (stars n, circle (fromIntegral n - 1) # lc (blend 0.95 white black))

stars :: Int -> Diagram Cairo R2
stars n = decoratePath
            (regP (fromIntegral n - 1))
            (map mkStar [0..])
  where
    mkStar 0    = decoratePath (regP 1) (repeat (circle 0.1 # lw 0))
                  # fc (colorByCosets n)
    mkStar skip = let p = star (StarSkip skip) (regP 1)
                      cosets = length . pathTrails $ p
                  in
                      p # stroke # lw 0.1 # lc (colorByCosets cosets)
    colorByCosets 1 = sRGB24 0xDA 0x22 0x22
    colorByCosets c = blend ((fromIntegral c - 2)/10) (sRGB24 0x4C 0x89 0xC0)
                                                      (sRGB24 0xD1 0xB3 0x41)
    regP r = polygon with { polyType   = PolyRegular n r
                          , polyOrient = NoOrient
                          }
           # rotateBy (1/4)

starsOfTheMindsSky = uncurry atop
                   . (mconcat***mconcat)
                   . unzip
                   . reverse
                   . map orbits
                   $ [2..24]

main = defaultMain (starsOfTheMindsSky # centerXY # pad 1.1)
