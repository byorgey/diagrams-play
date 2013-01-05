{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Path.Turtle

thing = runTurtle $ do
  setPenWidth 0.05
  replicateM_ 4 oneSide

oneSide = do
  forward 6
  lhook
  lhook
  rhook
  lhook
  lhook
  rhook
  right 90
 where
  lhook = left 90 >> forward 1
  rhook = right 90 >> forward 1

things = hcat' with { catMethod = Distrib, sep = 8 } (replicate 5 vstrip)
vstrip = vcat' with { catMethod = Distrib, sep = 8 } (replicate 5 thing)

main = defaultMain (things # bg grey)
