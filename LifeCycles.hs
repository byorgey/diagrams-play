{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Maybe
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

binarySearch :: (Fractional a, Ord a) => a -> a -> a -> (a -> Bool) -> Maybe a
binarySearch lo hi eps p
  | not (p lo) && p hi = Just $ binarySearch' lo hi
  | otherwise          = Nothing
  where
    binarySearch' lo hi
      | hi - lo <= eps = mid
      | p mid          = binarySearch' lo mid
      | otherwise      = binarySearch' mid hi
      where
        mid = (hi + lo) / 2

lifeCycleTemplate = mconcat circles <> arrowheads <> circle 2
  where
    circles = iterateN 4 (rotateBy (-1/4)) c
    c = circle 1 # fc white # translateX 2
    arrowhead
      = fromOffsets [unitX, unitY]
      # alignR
      # rotateBy (-1/8)
      # scale 0.25
    theta
      = fromJust
      $ binarySearch (0 :: Rad) (tau / 8) (1e-6)
          (not . getAny . sample (c :: D R2) . flip rotate (2 & 0))
    arrowheads
      = iterateN 4 (rotateBy (-1/4)) (arrowhead # translateX 2 # rotate theta)
      # mconcat

lifeCycle 0 = lifeCycleTemplate
lifeCycle n = iterateN 4 (rotateBy (1/4)) lifeCycle' # mconcat
           <> lifeCycleTemplate
  where
    lifeCycle' = lifeCycle (n-1) # scale (1/4) # translateX 2 # freeze

main = defaultMain $ lifeCycle 2 # sized (Width 1)
