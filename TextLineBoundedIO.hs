{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.Text

main = do
  txt <- textLineBoundedIO (mempty # fontSize 1) "iiiiiiiiii"
  print (width txt)