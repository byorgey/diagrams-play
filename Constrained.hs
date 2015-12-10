import Diagrams.Prelude
import Diagrams.TwoD.Layout.Constrained
import Diagrams.Backend.Rasterific.CmdLine
import Data.Maybe
import Control.Monad

-- dia :: Diagram B
-- dia = layout $ do
--   [squ, cir, cir2, d] <- intros [ square 2, circle 1, circle 1, circle 0.1 # fc black ]

-- --  a <- newAnchorOn squ (fromJust . rayTraceP origin (1 ^& 3))
--   constrain
--     [ centerAnchor squ .+^ (2 ^& 3) =.= centerAnchor cir
--     , centerAnchor cir .+^ (1 ^& (-4)) =.= centerAnchor cir2
--     , centerAnchor d =.= lerp 0.5 (centerAnchor squ) (centerAnchor cir2)
--     ]

dia :: Diagram B
dia = layout $ do
  cirs <- intros (map circle [1..5])
  sqs  <- intros (replicate 5 (square 2))
  constrainWith vcat cirs
  zipWithM_ sameX cirs sqs
--  zipWithM_ sameY sqs (tail sqs)

main = mainWith (dia # bg white)
