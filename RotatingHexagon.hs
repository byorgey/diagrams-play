import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import           Diagrams.LinearMap                  (vmap)
import           Diagrams.ThreeD.Projection
import           Linear.Matrix                       ((!*!))

hexa3 :: Path V3 Double
hexa3 = vmap (^& 0) (hexagon 1 :: Path V2 Double)

main = gifMain [ ( r # withEnvelope bb
                     # bgFrame 0.1 white
                 , 3
                 )
               | r <- allRots
               ]

allRots = [ hexa3 # transform (aboutY (r @@ turn)) # withPerspective
          | r <- [0, 1/100 .. 1/2]
          ]

bb = mconcat (map boundingBox allRots)

m  = lookAt (V3 8.4 6 3.2) zero unitZ
pm = perspective (pi/3) 0.8 (-10) 10 !*! m

pd = m44Deformation pm

withPerspective :: Path V3 Double -> Diagram B
withPerspective d = stroke $ deform pd (translateZ (-1) d)
