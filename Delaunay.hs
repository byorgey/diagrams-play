{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad
import           Control.Monad.Random
import qualified Data.Colour                    as Colour
import           Data.List
import qualified Data.Map                       as M
import           Data.Maybe                     (catMaybes)
import qualified Data.Set                       as S
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           Linear.Metric

type Pt = P2 Double

type Edge = (Pt,Pt)

type Triangle = [Pt]

delaunay :: [Pt] -> [Cell]
delaunay ps = filter (isDelaunay ps) . catMaybes . map mkCell . ksubsets 3 $ ps

isDelaunay :: [Pt] -> Cell -> Bool
isDelaunay ps (Cell [x,y,z] c) = all (\p -> (p `elem` [x,y,z]) || not (p `inCircle` c)) ps

{-
toEdges :: [Triangle] -> [Edge]
toEdges = map (toEdge . head) . group . sort . concatMap (ksubsets 2)
  where toEdge [x,y] = (x,y)
-}

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
data Circle = Circle Pt Double
  deriving (Eq, Ord, Show)

inCircle :: Pt -> Circle -> Bool
inCircle p (Circle ctr rSq) = quadrance (p .-. ctr) < rSq

mkCell :: [Pt] -> Maybe Cell
mkCell pts@[x,y,z] = Cell pts <$> circumcircle x y z
mkCell _ = Nothing

-- | Given three points, return their common circle, or Nothing if the
-- points are collinear (or very close).
circumcircle :: Pt -> Pt -> Pt -> Maybe Circle
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
    rSq   = quadrance (p1 .-. ctr)

    -- XXX can simplify/streamline the above?

-- | A cell is a triangle together with its circumcircle.
data Cell = Cell [Pt] Circle
  deriving (Eq, Ord, Show)

data Triangulation = Triangulation
  { triCells         :: [Cell]
  , triPointPointMap :: M.Map Pt (S.Set Pt)
  , triPointCellMap  :: M.Map Pt (S.Set Cell)
  , triEdgeCellMap   :: M.Map Edge (S.Set Cell)
  }

mkTriangulation :: [Cell] -> Triangulation
mkTriangulation = undefined

ksubsets :: Int -> [a] -> [[a]]
ksubsets 0 _  = [[]]
ksubsets n [] = []
ksubsets n (a:as) = map (a:) (ksubsets (n-1) as) ++ ksubsets n as

pointSet :: (Applicative m, MonadRandom m) => Int -> m [Pt]
pointSet n = replicateM n pt
--  where pt = (&) <$> getRandom <*> getRandom
  where pt = (\sr th -> ((sr * cos th) & (sr * sin th)) # translate (1&1) # scale 0.5)
               <$> (sqrt <$> getRandom)
               <*> getRandomR (0,tau)

drawTriangle (Cell vs c)
  = {- drawCircle c <> -}
    {- (mconcat . map dot $ vs) <> -}
    {- stroke tri # lc grey <> -}
    stroke tri
      {- # scaleAbout (avgX & avgY) 1.01 -}
      # lw 0 # fcA color
  where
    avgX = sum (map (fst . unp2) vs) / 3
    avgY = sum (map (snd . unp2) vs) / 3
    color = blend 0.5 (blue `withOpacity` avgX) (red `withOpacity` avgY)
    tri  = close $ fromVertices vs

    drawCircle (Circle ctr rSq) = circle (sqrt rSq) # moveTo ctr
    dot p = circle 0.01 # fc black # moveTo p

scaleAbout p d = scale d `under` translation (negate p)

heron [x,y,z] = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a+b+c)/2
    a = norm (x .-. y)
    b = norm (y .-. z)
    c = norm (z .-. x)

dia = delaunay ({- corners ++ -} circumf 30 ++ evalRand (pointSet 100) (mkStdGen 2))
    # map drawTriangle
    # mconcat
    # view origin (1&1)

circumf n = iterateN n (rotateBy (1/fromIntegral n)) (1&0)
          # map (scale 0.5 . translate (1&1))

corners = map (\[x,y] -> x & y) $ replicateM 2 [0,1]

main = defaultMain (dia # sized (Width 4) # centerXY # pad 1.1)
