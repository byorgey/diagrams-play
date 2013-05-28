{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.List
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

wiggle n s off = d blue pts1 <> d green pts2 <> p
  where
    dot  = circle 0.03
    d c  = mconcat . map (lw 0 . fc c . (dot#) . moveTo)
    p    = [pts1,pts2]
         # transpose
         # concat
         # cubicSpline True
         # fillRule EvenOdd
         # fc lightgray
         # lw 0
    pts1 = regPoly n 1
    pts2 = pts1 # scale s # rotateBy (off*(1/fromIntegral n))

a = (wiggle 7 1.3 <$> (interval 0.5 7.5)) <> pure (square 3.3 # fc white)

main = animMain (a # stretch 5)

--- main = defaultMain (wiggle 7 1.3 2.7 <> square 3.3)
