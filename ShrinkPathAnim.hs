{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad
import           Control.Monad.Random
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           ShrinkPath

randomTrail :: (MonadRandom m, Applicative m) => Int -> m (Trail R2)
randomTrail n = fromOffsets <$> replicateM n genStep
  where
    genStep = (&) <$> getRandomR (-1,1) <*> getRandomR (-1,1)

randomCycle :: (MonadRandom m, Applicative m) => Int -> m (Trail R2)
randomCycle n = do
  t <- randomTrail n
  let o = trailOffset t
      corr = negateV (o ^/ fromIntegral n)
  return . close . fromSegments . map (addOffset corr) . trailSegments $ t

addOffset o (Linear v) = Linear (v ^+^ o)
addOffset o (Cubic c1 c2 v) = Cubic c1 c2 (v ^+^ o)

main = do
  t <- evalRandIO (randomCycle 100)
  let p  = t # pathFromTrail
      ps = iterate (shrinkPath 0.5) p # take 1000
  animMain . stretchTo 20 . movie $ zipWith (\dia dur -> setEra (mkEra 0 dur) (pure dia)) (map (bg white . stroke) ps) durations

durations = iterate (\d -> 0.05 + ((d - 0.05) * 0.9)) 1
