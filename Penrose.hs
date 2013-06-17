{-# LANGUAGE NoMonomorphismRestriction #-}

-- http://preshing.com/20110831/penrose-tiling-explained
-- http://www.math.ubc.ca/~cass/courses/m308-02b/projects/schweber/penrose.html

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

data Tri
  = Small P2 P2 P2
  | Big   P2 P2 P2

mirror (Small a b c) = Small a c b
mirror (Big   a b c) = Big   a c b

drawTri :: Tri -> Diagram Cairo R2
drawTri (Small a b c) = drawTri' yellow b a c
drawTri (Big   a b c) = drawTri' purple b a c

drawTri' color x y z
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


divide :: Tri -> [Tri]
divide (Small a b c) =
  let p = alerp a b gr
  in  [Small c p b, Big p c a]
divide (Big a b c) =
  let q = alerp b a gr
      r = alerp b c gr
  in  [Big q r b, Small r q a, Big r c a]

gr :: Double
gr = -((1 - sqrt 5)/2)

triangles = zipWith ($) (cycle [id, mirror])
          $ zipWith (Small origin) d (tail (cycle d))
  where
    d = decagon 1 # rotateBy (-1/4)

drawTris = mconcat . map drawTri

step = concatMap divide

tilings = iterate step triangles

t n = drawTris (tilings !! n)
    # view ((-1) & (-1)) (2 & 2)

main = defaultMain (t 7)
