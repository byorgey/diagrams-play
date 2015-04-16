{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

sq = square 1 # lw none

envs :: Diagram B -> Diagram B
envs d = mconcat [ showEnvelope' (with & ePoints .~ 300) (d # lw none # translate pt) # translate (negated pt)
                 | x <- [-1.5, -0.5 .. 1.5]
                 , y <- [-1.5, -0.5 .. 1.5]
                 , let pt = x ^& y
                 ]

dia = (square 0.4 ||| triangle 0.5) === circle 0.2

main = mainWith (envs dia # frame 0.5)
