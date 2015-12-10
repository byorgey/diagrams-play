{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Data.List (genericLength)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import Data.Colour.Palette.BrewerSet

sjt 0 = [[]]
sjt n = concat $ zipWith ($) (cycle [map reverse . inserts (n-1) . reverse, inserts (n-1)]) (sjt (n-1))

inserts :: Int -> [Int] -> [[Int]]
inserts x [] = [[x]]
inserts x (y:ys) = (x:y:ys) : map (y:) (inserts x ys)

colors = drop 4 $ brewerSet Purples 9

type Perm = [Int]

drawPerm :: Perm -> Diagram B
drawPerm xs = hsep 0.5 (map (\i -> circle 1 # fc (colors !! i) # lw none) xs)

type Anim = (Double, Double -> Diagram B)

-- Given two Perms, find the *positions* that change.  Generate lists
-- with before & after positionsl; use them for positioning.  zip them
-- with actual sjt lists; use those for coloring.

-- Later: redo SJT generation so it directly generates *swaps* (in
-- terms of positions) rather than lists.  Easier to reconstruct lists
-- from swaps than vice versa, perhaps.

-- this assumes there is a single swap.
positionPerm :: Perm -> Perm -> [Int]
positionPerm xs1 xs2
  = [0 .. length xs1 - 1]
    # map (\x -> if x == x1 then x2 else (if x == x2 then x1 else x))
  where
    [x1,x2] = map fst . filter snd . zip [0..] $ zipWith (/=) xs1 xs2

animPermToPerm :: Perm -> Perm -> Anim
animPermToPerm xs1 xs2 =
  (1, \t -> (mconcat $ map (placeDot t) (zip (zip [0..] xs1) (zip (positionPerm xs1 xs2) xs2))) <> back)
  where
    back = square ((genericLength xs1 - 1) * dotPeriod + 2*dotRad + 2*dotSep)
         # fc white
         # translateX (dotPeriod * (genericLength xs1 - 1) / 2)
    dotSep = 1
    dotRad = 1
    dotPeriod = dotSep + 2*dotRad
    dot x = circle dotRad # fc (colors !! x) # lw none
    placeDot t ((i1,x1), (i2,x2)) = dot x1
      # rotateBy (if even (min i1 i2) then t/2 else -t/2) `underT`
          translationX
            ( dotPeriod/2
            * fromIntegral (signum (i1 - i2))
            )
      # translateX (dotPeriod * fromIntegral i1)

frames :: Int -> Anim -> [Diagram B]
frames fps (len, anim) = map anim [0, (1/fromIntegral fps) .. len]

emptyAnim :: Anim
emptyAnim = (0, const mempty)

(<+>) :: Anim -> Anim -> Anim
(len1, a1) <+> (len2, a2) = (len1 + len2, \t -> if t < len1 then a1 t else a2 (t - len1))

sjtAnim :: Int -> Anim
sjtAnim n = foldr (<+>) emptyAnim $ zipWith animPermToPerm perms (tail perms)
  where
    perms = sjt n

fps = 30 :: Int

main = mainWith (zip (frames fps (sjtAnim 4)) (repeat (100 `div` fps)))
