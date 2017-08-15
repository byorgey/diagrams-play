{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Maybe                          (fromJust)
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude                    hiding (dot)

import           Diagrams.TwoD.Path                  (isInsideWinding)

import qualified Diagrams.TwoD.Path.Boolean          as B

moon :: Path V2 Double
moon = B.difference Winding (circle 1) (circle 1 # translateX 0.7)

massCentroid2D :: Path V2 Double -> P2 Double
massCentroid2D = massCentroid2D' 0.02

massCentroid2D' :: Double -> Path V2 Double -> P2 Double
massCentroid2D' _ Empty = origin
massCentroid2D' f path  = centroid insidePts
  where
    Just (xlo, xhi) = extentX path
    Just (ylo, yhi) = extentY path
    grid = map p2 [(x,y) | x <- [xlo, ((1-f)*xlo + f*xhi) .. xhi]
                         , y <- [ylo, ((1-f)*ylo + f*yhi) .. yhi]]
    insidePts = filter (isInsideWinding path) grid

massCentroid :: (RealFloat n, Enum n) => Path V2 n -> Point V2 n
massCentroid = massCentroid' 0.02

massCentroid' :: (RealFloat n, Enum n) => n -> Path V2 n -> Point V2 n
massCentroid' _ Empty = origin
massCentroid' f path  = centroid insidePts
  where
    -- This can't return Nothing since we just checked the empty path
    -- case above
    grid          = fromJust $ boxGrid f (boundingBox path)
    insidePts     = filter (isInsideWinding path) grid

boxGrid
  :: (Traversable v, Applicative v, RealFloat n, Enum n)
  => n -> BoundingBox v n -> Maybe [Point v n]
boxGrid f b = (\(c1,c2) -> sequenceA (liftA2 mkRange c1 c2)) <$> getCorners b
  where
    mkRange lo hi = [lo, (1-f)*lo + f*hi .. hi]

dot :: Diagram B
dot = circle 0.02 # lw none

dotAt :: P2 Double -> Diagram B
dotAt pt = dot # moveTo pt

dia :: Diagram B
dia = mconcat
  [ moon # stroke # fcA transparent
  , dotAt (pathCentroid moon) # fc red
  , dotAt (massCentroid moon) # fc blue
  ]

main = do
  -- print (massCentroid2D 0.1 moon)
  -- print (pathCentroid moon)
  mainWith $ dia # frame 0.5


