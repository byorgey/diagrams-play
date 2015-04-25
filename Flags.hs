{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Colour.SRGB
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

main = mainWith $ dia # frame 0.1

dia :: Diagram B
dia = algeria

------------------------------------------------------------
-- Algeria
--
-- https://flagspot.net/flags/dz.html
------------------------------------------------------------

algeria = algeriaInnerCrescent # fc white <> algeriaOuterCrescent # fc red <> algeriaBack

-- Green: Pantone 356c / CMYK (%) 100-0-90-25
algeriaGreen = sRGB24 0 191 19

-- Red: Pantone 186c / CMYK (%) 0-90-80-5
algeriaRed = sRGB24 242 24 48

algeriaBack =
  hcat
  [ rect 15 20 # fc algeriaGreen # lw none
  , rect 15 20 # fc white # lw none
  ]
  # centerX

algeriaOuterCrescent, algeriaInnerCrescent :: Diagram B
algeriaOuterCrescent = circle 5
algeriaInnerCrescent = circlePtsR (algeriaCrescentPoints !! 0) (algeriaCrescentPoints !! 1) 4
algeriaCrescentPoints :: [P2 Double]
algeriaCrescentPoints = [ (5 ^& 0) # rotateBy r | r <- [-1/12, 1/12] ]


circlePtsR pt1 pt2 r = circle r # moveTo c
  where
    chord = pt2 .-. pt1
    d2 = quadrance chord
    theta = acosA (1 - d2 / (2 * r * r))
    c = pt1 .+^ (chord # rotate (fullTurn^/4 ^-^ theta^/2) # signorm # scale r)
