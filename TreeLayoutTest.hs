{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Layout.Tree

t = BNode 1 (BNode 8 (leaf 7) (leaf 2)) (BNode 6 (leaf 3) (leaf 4))

main = do
  let Just t' = uniqueXLayout 1 1 t
      t'' = forceLayoutTree defaultForceLayoutTreeOpts t'
      
  defaultMain $ renderTree (\n -> (text (show n) # fontSize 0.5 <> circle 0.3 # fc white)) (~~) t''