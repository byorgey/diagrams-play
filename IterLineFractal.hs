{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Colour.SRGB
import Data.List.Split

import Control.Applicative
import Control.Monad
import Control.Monad.Random

koch = fromOffsets [unitX, unitX # rotateBy (1/6), unitX # rotateBy (-1/6), unitX]
levy = fromOffsets [unitY, unitX]
zag  = fromOffsets [unitX, (-0.5) & 1, unitX]

sqUp     = fromOffsets [unitX, unitY, unitX, unit_Y, unitX]
sqUpDown = fromOffsets [unitX, unitY, unitX, 2 *^ unit_Y, unitX, unitY, unitX]

foozle   = cubicSpline False [origin, 1 & 1, 0 & 1, (-1) & 0]
c        = circle 1 # trailSegments # tail # fromSegments # scaleY 0.8

d = cubicSpline False sqUpDown

snowflake = iterateN 3 (rotateBy (-1/3)) edge
          # mconcat
          # strokeT
          # fc blue
          # lw 0
  where edge = iterTrail koch !! 5

-- dia = (iterTrail d !! 4 # strokeT) <> (lc gray . mconcat . map strokeT . take 5 $ iterTrail d)

trails = map strokeT . take 5 $ iterTrail d
dia = mconcat $ zipWith {- fc =) -} lc (iterate (blend 0.1 white) blue) trails

data IterTrailConfig = ITC { seed   :: Trail R2
                           , color  :: Colour Double
                           , iters  :: Int
                           }

randITC :: Rand StdGen IterTrailConfig
randITC = do
  numSegs <- getRandomR (2,5)
  spline  <- getRandom
  s       <- fromOffsets <$>
                replicateM numSegs ((&) <$> getRandomR (-1,1) <*> getRandomR (-1,1))
  c       <- sRGB <$> getRandom <*> getRandom <*> getRandom
  i       <- getRandomR (3, floor (logBase (fromIntegral numSegs) 10000))
  let s'
        | spline    = cubicSpline False s
        | otherwise = fromVertices s
  return $ ITC s' c i

drawITC :: IterTrailConfig -> Diagram Cairo R2
drawITC (ITC s c i) = (iterTrail s !! i) # strokeT # lc c

drawOne itc = drawITC itc
            # sized (Dims 4 4)
            # centerXY
            # pad 1.1

main = do
  itcs <- evalRandIO (replicateM 100 randITC)
  defaultMain (vcat . map hcat . chunksOf 10 . map drawOne $ itcs)

  -- (iterTrail d !! 4 # strokeT # sized (Width 4))

iterTrail :: Trail R2 -> [Trail R2]
iterTrail t = iterate (mconcat . map (refineSegment t) . trailSegments)
                      (fromOffsets [unitX])

refineSegment :: Trail R2 -> Segment R2 -> Trail R2
refineSegment t seg = t # scale k # rotateBy r
  where
    sOff = segOffset seg
    tOff = trailOffset t
    k    = magnitude sOff / magnitude tOff
    r    = direction sOff - direction tOff
