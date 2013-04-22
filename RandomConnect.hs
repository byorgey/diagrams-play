{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Random
import           Data.List
import           Data.List.Split
import           Data.Ord
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

rp :: (Applicative m, MonadRandom m) => m P2
rp = curry p2 <$> d100 <*> d100
  where d100 = getRandomR (0,100)

points :: (Applicative m, MonadRandom m) => Int -> m [P2]
points n = replicateM n rp

select :: [a] -> [(a,[a])]
select []     = []
select (x:xs) = (x,xs) : (map . second) (x:) (select xs)

closestPairs :: Int -> [P2] -> [(P2,P2)]
closestPairs n = map (uncurry $ pickClosest n) . select

pickClosest :: Int -> P2 -> [P2] -> (P2,P2)
pickClosest n p ps = (p, sortBy (comparing (\x -> magnitudeSq (p .-. x))) ps !! n)

dot = circle 0.5 # fc black

edges n = mconcat . map (uncurry (~~)) . closestPairs n

colors = [blue, red, green, purple]

connectDots dots pts n = (dots <> edges n pts # lc red)

main = do
  pts <- evalRandIO (points 100)
  print pts

{-
main = do
  pts <- evalRandIO (points 100)
  let dots  = position (zip pts (repeat dot))
      allEdges = mconcat $ zipWith lc colors (zipWith edges [0..] (repeat pts))
      connectGrid = vcat' with {sep = 10} . map (hcat' with {sep = 10}) . chunksOf 4 . map (connectDots dots pts) $ [0..3] ++ [10, 18 .. 98]
  defaultMain (connectGrid # sized (Width 3))
-}
