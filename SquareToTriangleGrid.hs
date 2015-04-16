{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List.Split
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

grid =
  mconcat
  [ vec (-1) 1
  , vec 0 1
  , vec 1 0
  , vec 1 (-1)
  , vec 0 (-1)
  , vec (-1) 0
  , replicate 4 (hcat $ replicate 4 (square 1)) # vcat # centerXY # lw thin # lc gray
  ]
  where
    vec x y = arrowAt origin (x ^& y)

dia = hcat' (with & catMethod .~ Distrib & sep .~ 3.5)
  [ grid
  , arrow 2 # lc blue # centerX
  , grid # shearX 0.5 # scaleY (sqrt 3 / 2)
  ]

main = defaultMain (dia # frame 0.5)
