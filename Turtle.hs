{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Path.Turtle

p = sketchTurtle t
d = drawTurtle t

t = do
      left 10
      forward 10
      right 90
      setPenColor blue
      forward 10
      left 80
      setPenColor orange
      forward 5

main = defaultMain (stroke p)