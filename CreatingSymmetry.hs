{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module CreatingSymmetry where

import           Control.Monad                       (replicateM)
import           Control.Monad.Random
import           Data.Complex
import           Data.List.Split                     (chunksOf)
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

type Params = [(Complex Double, Int)]

-- | Draw the parametric curve in the complex plane f(t) = \sum_{j=1}^M a_j e^{n_j i t} .
drawCyclicCurve :: (TrailLike t, N t ~ Double, V t ~ V2) => Params -> t
drawCyclicCurve = drawCyclicCurve' 360

-- | Draw the parametric curve in the complex plane f(t) =
--   \sum_{j=1}^M a_j e^{n_j i t}.  The first argument controls the
--   number of sample points to generate.
drawCyclicCurve' :: (TrailLike t, N t ~ Double, V t ~ V2) => Int -> Params -> t
drawCyclicCurve' samples params
  = cubicSpline True (map (eval params) [0, incr .. tau - incr])
  where
    incr = tau / fromIntegral samples
    eval params t = c2p . sum . map (evalComponent t) $ params
    evalComponent t (a,n) = a * cis (fromIntegral n * t)
    c2p (x :+ y) = (x ^& y)

randomCurve :: (MonadRandom m, Applicative m) => m Params
randomCurve = do
  numComponents <- getRandomR (2, 4)
  replicateM numComponents randomComponent

randomComponent :: (MonadRandom m, Applicative m) => m (Complex Double, Int)
randomComponent = (,) <$> ((:+) <$> getRandomR (-1,1) <*> getRandomR (-1,1)) <*> getRandomR (-30,30)

mysteryCurve :: Diagram B
mysteryCurve = drawCyclicCurve [(1,1), (1/2, 6), (0 :+ 1/3, -14)]

randomCurves :: Int -> IO (Diagram B)
randomCurves n = do
  curves <- evalRandIO $ replicateM n randomCurve
  return
    $ vsep 1 . map (hsep 1) . chunksOf 5
    . map (sized (dims2D 4 4) . drawCyclicCurve)
    $ curves

-- main = do
--   dia' <- dia
--   mainWith $ dia' # lw veryThin # frame 1 # bg white
