{-# LANGUAGE NoMonomorphismRestriction #-}

module RenderTree where

import Data.Maybe (fromMaybe)
import System.Environment (withArgs)
import Data.Tree (Tree(Node))
import Diagrams.Prelude
import Diagrams.Backend.Cairo.Text (textLineBoundedIO)
import Diagrams.TwoD.Layout.Tree (renderTree, symmLayout', slHSep, slVSep, slWidth, slHeight)
import Diagrams.Backend.Cairo.CmdLine (defaultMain)

lf x = Node x []

t1 = Node "Root" [Node "Left" (map lf ["New York","Washington","Los Angeles"]),
                  Node "Right" [Node "Extended\nEntry" (map lf ["H","I","J"])]]

dtree (Node x children) =
  do let ls = lines x
     ys <- mapM (textLineBoundedIO (fontSize 0.5)) ls
     let y = vcat . map centerX $ ys
     ts <- mapM dtree children
     let (w,h) = size2D y
     return (Node (y # centerXY <> roundedRect (w + 1) (h + 0.5) 0.2 # fc white) ts)

renderTreeToFile tree filename =
  do tree' <- dtree tree
     let d = renderTree
               id
               (~~)
               (symmLayout' with { -- slHSep = 2.25
                                 -- , slVSep = 2.25
                                 -- ,
                                   slWidth = fromMaybe (0,0) . extentX
                                 , slHeight = fromMaybe (0,0) . extentY
                                 }
                tree'
               )
     withArgs ["-o",filename,"-w","400"] (defaultMain d)

test = renderTreeToFile t1 "test.pdf"
