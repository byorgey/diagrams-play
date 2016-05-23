import           Control.Monad                       (zipWithM_)
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Constrained

dia :: Diagram B
dia = layout $ do
  sqs  <- newDias (replicate 5 (square 2) # fc orange)
  cirs <- newDias (map circle [1..5] # fc blue)
  constrainWith vcat cirs
  zipWithM_ sameY cirs sqs
  constrainWith hcat [cirs !! 0, sqs !! 0]
  along (direction (1 ^& (-1))) (map centerOf sqs)

-- dia2 :: Diagram B
-- dia2 = layout $ do
--   cir <- newDia (circle 1)
--   sqs <- newDias (map square [1..5])
--   constrainWith (hsep 0.3) sqs
--   lc <- newPointOn cir (envelopeP unit_X)
--   rc <- newPointOn cir (envelopeP unitX)
--   ls <- newPointOn (head sqs) (envelopeP unitX)
--   rs <- newPointOn (last sqs) (envelopeP unit_X)
--   mapM_ fixScale sqs
--   constrain $ do
--     lc =.= ls
--     rc =.= rs

-- dia3 :: Diagram B
-- dia3 = layout $ do
--   bigSq <- newDia (square 14.3)
--   fixScale bigSq

--   lb <- newPointOn bigSq (envelopeP unit_X)
--   rb <- newPointOn bigSq (envelopeP unitX)

--   sqs <- newDias (replicate 5 (square 1))

--   ls <- newPointOn (head sqs) (envelopeP unit_X)
--   rs <- newPointOn (last sqs) (envelopeP unitX)

--   lb =.= ls
--   rb =.= rs

-- -- doesn't work --- causes weird output
-- --  allSame (map scaleOf sqs)

--   constrainWith (hsep 0.3) sqs

-- dia4 :: Diagram B
-- dia4 = layout $ do
--   cirs <- newDias (replicate 5 (square 1))
--   constrainWith (hsep 0.3) cirs

dia5 :: Diagram B
dia5 = layout $ do
  cirs <- newDias (map circle [1..5])
  constrainWith (hsep 1) cirs
  rc <- newPointOn (last cirs) (envelopeP unitX)

  sq <- newScalableDia (square 1)
  ls <- newPointOn sq (envelopeP unit_X)
  rs <- newPointOn sq (envelopeP unitX)

  ls =.= centerOf (cirs !! 2)
  rs =.= rc

dia6 :: Diagram B
dia6 = layout $ do
  c2 <- newScalableDia (circle 2)
  p <- newPointOn c2 (const $ (1 ^& 0) # rotateBy (1/8))

  c1 <- newDia (circle 1)
  centerOf c1 =.= p
  scaleOf c2 ==== 2

  [a,b] <- newDias (replicate 2 (circle 1.5))
  constrainWith hcat [a,c2,b]

main = mainWith (dia6 # frame 1 # bg white)
