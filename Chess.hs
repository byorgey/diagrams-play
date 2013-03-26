{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

board n c =
  mconcat [ square 1 # lw 0 # fc (c i j) # translate (fromIntegral i & fromIntegral j)
          | i <- [0..n-1]
          , j <- [0..n-1]
          ]

checkersColors = map (blend 0.2 white) [red, black]

main = defaultMain (board 8 (\i j -> checkersColors !! ((i+j) `mod` 2)))