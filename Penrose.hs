{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}

-- http://preshing.com/20110831/penrose-tiling-explained
-- http://www.math.ubc.ca/~cass/courses/m308-02b/projects/schweber/penrose.html
-- http://en.wikipedia.org/wiki/Penrose_tiling

-- See also http://www.math.brown.edu/~res/MFS/handout7.pdf for some proofs

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

-- Type A triangles make up kites + darts; Type B triangles make up rhombs.
data RobinsonType = A | B

-- Robinson triangles.
data Tri :: RobinsonType -> * where
  Acute  :: P2 -> P2 -> P2 -> Tri r
  Obtuse :: P2 -> P2 -> P2 -> Tri r

mirror :: Tri r -> Tri r
mirror (Acute  a b c) = Acute  a c b
mirror (Obtuse a b c) = Obtuse a c b

class Drawable a where
  draw :: a -> Diagram Cairo R2

instance Drawable (Tri A) where
  draw (Acute  a b c) = drawTri purple a b c
  draw (Obtuse a b c) = drawTri yellow a b c

instance Drawable (Tri B) where
  draw (Acute  a b c) = drawTri yellow b a c
  draw (Obtuse a b c) = drawTri purple b a c

drawTri :: Colour Double -> P2 -> P2 -> P2 -> Diagram Cairo R2
drawTri color x y z
  =
  ( stroke (fromVertices [x,y,z])
  # fc color
  # lineJoin LineJoinRound
  # lw 0
  )
  -- <>
  -- ( z ~~ x
  -- # lc color
  -- )

-- Inflation of Robinson triangles to generate a P2 (kites + darts) Penrose tiling.
inflateP2 :: Tri A -> [Tri A]
inflateP2 (Acute a b c) =
  let q = alerp b a gr
      r = alerp a c gr
  in  [Obtuse q r a, Acute b q r, Acute b c r]
inflateP2 (Obtuse a b c) =
  let p = alerp c b gr
  in  [Obtuse p a b, Acute c p a]

-- Inflation of Robinson triangles to generate a P3 (rhombus) Penrose tiling.
inflateP3 :: Tri B -> [Tri B]
inflateP3 (Acute a b c) =
  let p = alerp a b gr
  in  [Acute c p b, Obtuse p c a]
inflateP3 (Obtuse a b c) =
  let q = alerp b a gr
      r = alerp b c gr
  in  [Obtuse q r b, Acute r q a, Obtuse r c a]

gr :: Double
gr = -((1 - sqrt 5)/2)

triangles :: [Tri r]
triangles = zipWith ($) (cycle [id, mirror])
          $ zipWith (Acute origin) d (tail (cycle d))
  where
    d = decagon 1 # rotateBy (-1/4)

drawTris = mconcat . map draw

stepA = concatMap inflateP2
stepB = concatMap inflateP3

tilingsA = iterate stepA triangles
tilingsB = iterate stepB triangles

t ts n = drawTris (ts !! n)
       # view ((-1) & (-1)) (2 & 2)

main = defaultMain (t tilingsA 7)
