{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Diagrams.TwoD.Factorization

lStar = l
    # translateY 0.5
    # cyc 6
    # atop (circle 0.75 # lw 0)
  where
    l = text "L" <> text "y" # scale 0.2 # translateY (-0.4)
                 <> text "n" # scale 0.4 # translateY 0.35 # translateX (-0.25)

sierpinski 0 = lStar # sized (Width 1)
sierpinski n = t' # cyc 3 # sized (Width 1)
     <> centerDeco n
  where t' = sierpinski (n-1) # translateY 0.6

centerDeco 1 = mempty
centerDeco 2 = tt # translateY 0.05 # cyc 6
  where t = text "t" # translateX 0.15
        tt = (t <> reflectX t) # scale 0.1
centerDeco 3 = text "e" # translateY 0.5 # scale 0.15 # cyc 6
            <> text "e" # translateY 1.5 # scale 0.1  # cyc 3 # rotateBy (1/6)

cyc n = mconcat . iterateN n (rotateBy (1/fromIntegral n))

main = defaultMain (sierpinski 3 # pad 1.1)
