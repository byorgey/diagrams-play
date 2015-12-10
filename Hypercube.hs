{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Core.Transform             (fromOrthogonal)
import           Diagrams.Prelude

import           Linear.V4

type P4 = Point V4
type T4 = Transformation V4

type instance V (V4 n) = V4
type instance N (V4 n) = n

instance Transformable (V4 n) where
  transform = apply

aboutXY :: Floating n => Angle n -> Transformation V4 n
aboutXY (view rad -> a) = fromOrthogonal r where
  r = rot a <-> rot (-a)
  rot θ (V4 x y z w) = V4 x y
                        (cos θ * z - sin θ * w)
                        (sin θ * z + cos θ * w)

box :: Path V4 Double
box = Path [f p1 ~~ f p2 | p1 <- ps, p2 <- ps
                         , quadrance (p1 .-. p2) == 4
                         , p1 < p2   -- Only generate one copy of each edge
           ]
  where
    ps = getAllCorners $ fromCorners (-1) 1
    f  = fmap fromIntegral
