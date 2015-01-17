{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

example = text "Hello World!" # fontSize 0.2 # scaleX 2 <> rect 3 1 # lwO 1 # showOrigin

main = defaultMain example
