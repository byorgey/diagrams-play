{-# LANGUAGE NoMonomorphismRestriction, GADTs, TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

eqTri = regPoly 3 1 # fc green # scale 0.9 # pad (1/0.9)

hex = mconcat . take 6 . iterate (rotateBy (1/6)) $ t
  where t = eqTri # centerX # alignT

sq = square 1 # fc blue # scale 0.9 # pad (1/0.9)

sqTri = (sq ||| eqTri # rotateBy (-1/4)) # alignB

data Pair a b = Pair a b
type instance V (Pair a b) = V a
instance (Transformable a, Transformable b, V a ~ V b) => Transformable (Pair a b) where
  transform t (Pair a b) = Pair (transform t a) (transform t b)

dodecTiling = lw 0.03 . lineJoin LineJoinRound . appends hex . map unPair . iterateN 6 (rotateBy (1/6)) $ Pair unitY sqTri
  where unPair (Pair a b) = (a,b)

iterateN n f = take n . iterate f

main = defaultMain dodecTiling