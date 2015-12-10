{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.GraphViz

import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Commands

hex = mkGraph [0..19]
        (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
         ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
         ++ [ (2,18,()), (2,19,()), (15,18,()), (15,19,()), (18,3,()), (19,3,()) ]
        )

main = do
  let params :: GraphvizParams Int v e () v
      params = defaultDiaParams
               { fmtEdge = const [arrowTo noArrow] }
  hex' <- layoutGraph' params Dot hex
  let hexDrawing :: Diagram B
      hexDrawing = drawGraph
                     (const $ place (circle 19))
--                     (\_ _ _ _ _ p -> stroke p)
                     (\v1 p1 v2 p2 e p -> (text ("Edge " ++ show v1 ++ " " ++ show v2) # fontSize 5 # moveTo (labelPt p)) <> (arrowBetween' (opts p) p1 p2))
                     hex'
      opts p = with & gaps .~ 16 & arrowShaft .~ (unLoc . head $ pathTrails p)
      labelPt p = ltr `atParam` 0.5 .+^ 20 *^ n
        where
          ltr = head (pathTrails p)
          n = ltr `normalAtParam` 0.5
  mainWith $ hexDrawing # frame 1
