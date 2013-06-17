{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.List
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

wiggle n s off = d blue pts1 <> d green pts2 <> p
  where
    dot  = circle 0.03
    d c  = mconcat . map (lw 0 . fc c . (dot#) . moveTo)
    p    = cubicSpline True . concat . transpose $ [pts1,pts2]
    pts1 = regPoly n 1
    pts2 = pts1 # scale s # rotateBy (off*(1/fromIntegral n))

main = defaultMain (wiggle 6 1.2 0.5)
