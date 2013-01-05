{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Maybe
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Combinators

type DC = Diagram Cairo R2

runEnvelopeTest :: DC -> DC
runEnvelopeTest = lw 0.05 . sampleEnvelope2D 100

sampleEnvelope2D :: Int -> DC -> DC
sampleEnvelope2D n d = foldr (flip atop) (d # lc red) bs
    where b  = fromJust $ appEnvelope (envelope d)
          bs :: [DC]
          bs = [stroke $ mkLine (origin .+^ (s *^ v)) (perp v) | v <- vs, let s = b v]
          vs = map r2 [ (2 * cos t, 2 * sin t)
                      | i <- [0..n]
                      , let t = ((fromIntegral i) * 2.0 * pi) / (fromIntegral n)
                      ]
          mkLine a v = (moveTo a $ fromOffsets [v] # centerXY)
          perp (unr2 -> (x,y)) = r2 (-y,x) # scale 5

d = square 2 # rotateBy (1/8) # padR2 (r2 (1,2))

main = defaultMain (runEnvelopeTest $ d)