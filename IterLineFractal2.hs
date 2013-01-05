{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Path.IteratedSubset

main = defaultMain ((iterTrail sqUp !! 5) # strokeT # sized (Width 4))
