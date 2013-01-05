{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmF she #-} 

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Colour

wheel = (circle 0.05 # fc red # lw 0 # translateY (-1))
        <>
        circle 1

cycloid :: Animation Cairo R2
cycloid = (|lc (| blend ui ~red ~blue |)
               (| rotate (| Rad (ui # scale (-tau)) |) ~wheel |)
          |)

main = animMain (cycloid <> square 2.2 # lw 0 # fc white)
