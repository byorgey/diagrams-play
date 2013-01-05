{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Data.Tree

lf x = Node x []
t1 = Node 'A' [Node 'B' (map lf "CDE"), Node 'F' [Node 'G' (map lf "HIJ")]]

example =
  renderTree ((<> circle 1 # fc white) . text . show)
             (~~)
             (symmLayout' with { slHSep = 4, slVSep = 4 } t1)