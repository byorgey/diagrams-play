{- Harriss spiral.

   Rectangle

   +-----------------+
   |                 |
 1 |                 |
   |                 |
   |                 |
   +-----------------+
            p

   divides into 2 rectangles + square, both rectangles are similar to original:

   +------+----------+
   |      |   r2     |
   |      +----------|
 1 |  r1  |          |
   |      |  square  |
   +------+----------+
      1/p    p - 1/p

   Let the rectangle be 1 by p units.  Then

   1/p = (1-(p-1/p))/(p - 1/p)   (big rectangle similar to r2)

   Solving for p,

   p - 1/p = p - p^2 + 1
   p^2 - 1 = p^2 - p^3 + p
   p^3 - p - 1 = 0

   So p =~ 1.3247179572447458  (plastic number).

-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Diagrams.Solve.Polynomial

plastic :: Double
[plastic] = cubForm 1 0 (-1) (-1)

plasticRect :: Located (Trail' Loop V2 Double)
plasticRect = fromOffsets
  [ plastic *^ unitX
  ,            unitY
  , plastic *^ unit_X
  ,            unit_Y
  ]

theArc :: Located (Trail' Line V2 Double)
theArc
  = arc xDir (1/4 @@ turn)
  # rotateBy (-3/8)
  # scale ((1/plastic) / sqrt 2)
  # translate ((1/plastic/2) ^& (1/plastic/2))

t1 :: T2 Double
t1 = translationX (1/plastic) <> scaling (1/plastic) <> rotation (1/4 @@ turn)

t2 :: T2 Double
t2 = translation ((1/plastic) ^& (plastic - 1/plastic)) <> scaling (1 - 1/plastic^2)

theUnit :: Diagram B
theUnit = stroke theArc <> stroke plasticRect

harriss :: Int -> Diagram B
harriss n
  | n <= 0    = mempty
  | otherwise = stroke theArc <> transform t1 (harriss (n-1)) <> transform t2 (harriss (n-1))

dia :: Diagram B
dia = harriss 10

main = mainWith (dia # frame 0.1)
