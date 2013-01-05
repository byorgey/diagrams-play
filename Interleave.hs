{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmF she #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

sq  = square 1 # fc blue # lw 0.02

row s = hcat' with { sep = s } (replicate 5 sq)

s = mkActive 0 1 (\t -> (cos (fromTime t * tau/2 + tau/2) + 1) / 2 + 0.1)

row1 = (| row s |)
row2 = (| translateX ui row1 |)

spread = row2 === pure (strutY 1) === row1

a1 = stretch 3 spread 

merge = 
  (
        (| translateY (| ~(-2) + (interval 0 1) |) ~(activeEnd row1) |)
     <> (| translateY (| negate (interval 0 1) |) ~(activeEnd row2) |)
  ) 
  # stretch 2

theAnim = movie [a1, merge]

-- better idea: use "keyframes" with named subdiagrams, and
-- interpolate between them.

addBG :: Animation Cairo R2 -> Animation Cairo R2
addBG a = a <> pure (animRect a 
                      # expandPath 1.2 
                      # stroke 
                      # fc white
                    )

main = animMain $ addBG theAnim