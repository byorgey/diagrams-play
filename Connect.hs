{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.List
import           Data.Ord
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

box1 = square 1 # named "box1"
box2 = rect 3 2 # named "box2"

boxes = box1 <> box2 # translate (4 & 3)

magnify = boxes # (withNames ["box1", "box2"] $ \bs ->
  let [cs1, cs2] = map (getAllCorners . boundingBox . getSub) bs
      ul         = maximumBy (comparing (uncurry (flip (-)) . unp2))
      lr         = maximumBy (comparing (uncurry (-) . unp2))
  in  (atop $
            (ul cs1 ~~ ul cs2)
         <> (lr cs1 ~~ lr cs2)
      )
  )

main = defaultMain (magnify # centerXY # pad 1.1)
