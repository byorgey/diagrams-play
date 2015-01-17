{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

dia = arrow 1
   <> square 2

main = defaultMain (dia # hsO 200 # centerXY # pad 1.1)
