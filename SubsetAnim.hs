{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmF she #-}

import Data.Traversable
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Path.IteratedSubset

pR = (| (| cos theta |) & (| sin theta |) |)
  where theta = (| ui * ~tau |)

pA = pR # translateX (-2)

pB = pR # reflectX # translateX 2 # stretch 2

drawPt = fmap $ place (circle 0.1 # fc white)

t = (strokeT . (!!4) . iterTrail . fromVertices) <$> sequenceA [pure ((-1) & 1), pA, pB, pure (1 & 0)]

main = animMain $ (t # lc white) # stretch 3