{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

letter x = square 1 <> text [x]

main = defaultMain (hcat . map letter $ "abcdefg")