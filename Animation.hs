{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

s = bezier3 (0,2) (3,3) (2,0)

toDouble = fromRational . toRational

a = mkActive 0 1 (\t -> circle 0.3 # fc black # translate (atParam s (toDouble t)))

main = animMain ((const <$> (a # stretch 3) <*> interval (-1) 4) <> square 5 # fc white # lw 0)