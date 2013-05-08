{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad
import           Control.Monad.Random
import qualified Data.Colour                    as Colour
import           Data.List
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

type Edge = (P2,P2)

type Triangle = [P2]

check :: a -> (a -> a)
check _ = id

delaunay :: [P2] -> [Triangle]
delaunay ps = filter isDelaunay . ksubsets 3
            $ ps
  where
    isDelaunay [x,y,z] =
      let mc = circumcircle x y z
      in  case mc of
            Nothing -> False
            Just c  -> all (\p -> (p `elem` [x,y,z]) || not (p `inCircle` c)) ps

toEdges :: [Triangle] -> [Edge]
toEdges = map (toEdge . head) . group . sort . concatMap (ksubsets 2)
  where toEdge [x,y] = (x,y)

{-

  Ah, of course -- intersection of segment bisectors.

  l1 = (p1 + p2)/2 + t (p2 - p1)^
  l2 = (p2 + p3)/2 + s (p3 - p2)^

  If p2 - p1 and p3 - p2 are parallel (or close to it), reject.  That
  means the points are collinear or nearly so (in which case the
  circumcircle is degenerate or very large).

  Otherwise, solve for s & t:

  (p1 + p2)/2 + t (p2 - p1)^ = (p2 + p3)/2 + s (p3 - p2)^

  (p1 + p2)/2 - (p2 + p3)/2 + t (p2 - p1)^ - s (p3 - p2)^ = 0
  ((p1 + p2)/2 - (p2 + p3)/2) + [(p2 - p1)^ (p3 - p2)^] [t s]^T = 0

  This is a matrix equation.  Solution for [t s] is

  [(p2 - p1)^ (p3 - p2)^]^-1 ((p2 + p3)/2) - (p1 + p2)/2)

  i.e. let

  [ a b ] = [(p2 - p1)^ (p3 - p2)^]
  [ c d ]

  and  (x,y) = ((p2 + p3)/2) - (p1 + p2)/2)

  then we have

  t = (dx - by)/(ad - bc)
  s = (-cx + ay)/(ad - bc)

-}

-- | A circle, represented as center + squared radius.
data Circle = Circle P2 Double

inCircle :: P2 -> Circle -> Bool
inCircle p (Circle ctr rSq) = magnitudeSq (p .-. ctr) < rSq

-- | Given three points, return their common circle, or Nothing if the
-- points are collinear (or very close).
circumcircle :: P2 -> P2 -> P2 -> Maybe Circle
circumcircle p1 p2 p3
  | abs det < 0.00001 = Nothing
  | otherwise         = Just $ Circle ctr rSq
  where
    v1    = p2 .-. p1
    v2    = p3 .-. p2
    perp  = rotateBy (1/4)
    (a,c) = v1 # perp # unr2
    (b,d) = v2 # perp # unr2
    (x,y) = unr2 (alerp p2 p3 0.5 .-. alerp p1 p2 0.5)
    det   = a*d - b*c
    t     = (d*x - b*y) / det
    s     = (-c*x + a*y) / det
    ctr   = alerp p1 p2 0.5 .+^ (perp v1 # scale t)
    rSq   = magnitudeSq (p1 .-. ctr)

    -- XXX can simplify/streamline the above?

ksubsets :: Int -> [a] -> [[a]]
ksubsets 0 _  = [[]]
ksubsets n [] = []
ksubsets n (a:as) = map (a:) (ksubsets (n-1) as) ++ ksubsets n as

pointSet :: (Applicative m, MonadRandom m) => Int -> m [P2]
pointSet n = replicateM n pt
  where pt = (&) <$> getRandom <*> getRandom

drawTriangle vs = {- stroke tri # lc grey <> -} stroke tri {- # scaleAbout (avgX & avgY) 1 -} # lw 0 # fcA color
  where
    avgX = sum (map (fst . unp2) vs) / 3
    avgY = sum (map (snd . unp2) vs) / 3
    color = blend 0.5 (blue `withOpacity` avgX) (red `withOpacity` avgY)
    tri  = close $ fromVertices vs

scaleAbout p d = scale d `under` translation (negate p)

heron [x,y,z] = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a+b+c)/2
    a = magnitude (x .-. y)
    b = magnitude (y .-. z)
    c = magnitude (z .-. x)

dia = mconcat . map drawTriangle
    $ delaunay (evalRand (pointSet 100) (mkStdGen 1))

corners = map (\[x,y] -> x & y) $ replicateM 2 [0,1]

main = defaultMain (dia # sized (Width 4) # centerXY # pad 1.1)
