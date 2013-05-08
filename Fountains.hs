{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Unamb (unamb)
import Data.Universe.Helpers (diagonal, (+*+))
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Layout.CirclePacking

data Nat = Z | S Nat

data SNat :: Nat -> * where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

toInt :: SNat n -> Int
toInt SZ     = 0
toInt (SS n) = 1 + toInt n

type Fountain = Fountain' (S Z)

data Fountain' :: Nat -> * where
  F :: [(SNat n, Fountain' (S n))] -> Fountain' n

f :: Fountain
f = F [(SS SZ, F [(SS (SS SZ), F [])])]

fountainToSequence :: Fountain' n -> [Int]
fountainToSequence (F blocks) = concatMap blockToSequence blocks
  where
    blockToSequence (n, f) = toInt n : fountainToSequence f

drawFountain :: Renderable (Path R2) b => Fountain -> Diagram b R2
drawFountain = drawSequence . fountainToSequence
  where
    drawSequence = hcat' with {catMethod = Distrib, sep = 1} . map drawRow
    drawRow n = take n (map ((circle 0.5#) . fc) (cycle [blue, green, red, yellow]))
              # hcat
              # rotateBy (1/3)

-- Doing it directly via generating functions?

type GS a = [[a]]

(.++) :: GS a -> GS a -> GS a
(.++) = zipWith (++)

(.+.) :: GS a -> GS b -> GS (Either a b)
(.+.) = zipWith (\as bs -> map Left as ++ map Right bs)

(.*.) :: GS a -> GS b -> GS (a,b)
(as:ass) .*. b = cp as (head b) : (as .* tail b) .++ (ass .*. b)

(.*) :: [a] -> GS b -> GS (a,b)
as .* bss = sumGS $ map (\a -> (map . map) ((,) a) bss) as

-- Doubly lazy cartesian product

-- XXX generalize CP etc. to work over any sort of product.  Then we
-- can use it for cp itself as well as (.*.) above.

cp as bs = unamb (as +*+ bs) (bs *+* as)

[] *+* _  = []
xs *+* ys = map swap . diagonal . transpose $ [[(x,y) | x <- xs] | y <- ys]

sumGS :: [GS a] -> GS a
sumGS = foldr (.++) zeroGS

mapGS :: (a -> b) -> GS a -> GS b
mapGS = map . map

zeroGS :: GS a
zeroGS = repeat []

oneGS :: GS ()
oneGS = [()] : zeroGS

list :: GS a -> GS [a]
list g = mapGS iso $ oneGS .+. (g .*. list g)
  where
    iso :: Either () (a,[a]) -> [a]
    iso (Left ()) = []
    iso (Right (x,xs)) = x : xs

fountains :: GS Fountain
fountains = fountains' (SS SZ)

fountains' :: SNat n -> GS (Fountain' n)
fountains' n = mapGS F $ list (z n .*. fountains' (SS n))
  where
    z :: SNat n -> GS (SNat n)
    z n = replicate (toInt n) [] ++ [[n]] ++ zeroGS

dia = hcat' with {sep=2}
    . map (vcat' with {sep=1})
    . (map . map) (centerX . drawFountain)
    . redistrib
    . take 11
    $ fountains

{-
dia = renderCirclePacking ((0.5+) . approxRadius 8)
    . zipWith rotateBy [0, 0.01 ..]
    . map (centerXY . drawFountain)
    . concat
    . take 11
    $ fountains
-}

redistrib xs = chunksOf n xs'
  where
    xs' = concat xs
    n   = ceiling (sqrt (fromIntegral (length xs')))

main = defaultMain (dia # centerXY # pad 1.1 # sized (Width 10))
