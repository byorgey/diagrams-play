
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Default

import Data.Tree
import Tree

{-
instance Semiring Any where
  one = Any True
  Any x <.> Any y = Any (x && y)
-}

t :: Tree Int
t = Node 1 [Node 2 [], Node 3 [Node 5 [Node 8 [], Node 9 []]], Node 4 [Node 6 [], Node 7 []]]

d = renderTree def (\n -> text (show n) <> square 1 # fc white) t
--  # withNameAll (Child 3) (\ps -> atop (mconcat (map (dot . fst) ps)))
  
-- dot p = circle 0.2 # fc red # moveTo p

main = defaultMain d