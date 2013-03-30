module ShrinkPath where

import           Diagrams.Prelude

shrinkPath :: VectorSpace v => Scalar v -> Path v -> Path v
shrinkPath s = close . mconcat . map (fromVertices . shrinkVertices s) . pathVertices

shrinkVertices :: VectorSpace v => Scalar v -> [Point v] -> [Point v]
shrinkVertices s ps = take l . (zipWith (\p1 p2 -> alerp p1 p2 s) <*> tail) . cycle $ ps
  where l = length ps
