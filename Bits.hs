{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad
import           Data.List.Split
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

colors = map (blend 0.3 white) (cycle [white, yellow, orange, red, blue, green, purple, brown, black])

drawBits bs =
  (  circle 1
  <> mconcat
     ( zipWith (\b r -> if b then r else mempty) bs
         (iterate (rotateBy (1/fromIntegral (length bs * 2))) (hrule 2))
     )
  <> square 2.4
     # fc (colors !! length (filter id bs))
     # lw 0
  )
  # lw 0.1

main
  = defaultMain
  . pad 1.1
  . centerXY
  . vcat . map hcat
  . chunksOf (2 ^ ((n+1) `div` 2))
  . map drawBits
  . replicateM n
  $ [False,True]

n = 8
