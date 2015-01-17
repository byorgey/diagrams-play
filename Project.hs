{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Diagrams.Project
import           Diagrams.TwoD.Project

ptMark :: Diagram SVG R2
ptMark = circle 0.02 # lw 0

illustrateSegment :: FixedSegment R2 -> Diagram SVG R2
illustrateSegment (FLinear from to) = position [
    (from, ptMark # fc blue),
    (to,   ptMark # fc blue)]
illustrateSegment (FCubic from c1 c2 to) = position [
    (c1, ptMark # fc red),
    (c2, ptMark # fc red)] <> illustrateSegment (FLinear from to)

illustratePath t =
    stroke t
    <> (mconcat . map illustrateSegment . concatMap fixTrail . pathTrails $ t)

sq :: Path R2
sq = unitSquare # translate (5 ^& 3)

shaft = arc 0 (1/8 @@ turn)

curvedArrow = arrowBetween' (with & arrowShaft .~ shaft
                                  & shaftStyle %~ lc gray
                                  & headStyle  %~ fc gray
                            )

dashedLine :: P2 -> P2 -> Diagram SVG R2
dashedLine from to = fromVertices [from, to] # dashing [0.1, 0.1] 0

illustrateProjection :: Projection R2 -> Path R2 -> ([P2], [P2], Diagram SVG R2)
illustrateProjection t s = (basePts, projectedPts, projectedMarks) where
    basePts        = concat $ pathVertices sq
    projectedPts   = map (project t) basePts
    projectedMarks = position $ zip projectedPts (repeat (ptMark # fc green))

x0 = mconcat $ [illustratePath sq, projectedMarks]
               ++ zipWith curvedArrow basePts projectedPts
               ++ zipWith dashedLine basePts projectedPts
  where
    (basePts, projectedPts, projectedMarks) = illustrateProjection parallelX0 sq

x1 = mconcat $ [illustratePath sq, projectedMarks]
               ++ zipWith curvedArrow basePts projectedPts
               ++ zipWith dashedLine basePts (repeat origin)
  where
    (basePts, projectedPts, projectedMarks) = illustrateProjection perspectiveX1 sq

y0Rotated = mconcat $ [illustratePath sq, projectedMarks]
               ++ zipWith curvedArrow basePts projectedPts
  where
    parallelY0' = asProjection (rotation (-1/4 @@ turn)) <> parallelX0 <> asProjection (rotation (1/4 @@ turn))
    (basePts, projectedPts, projectedMarks) = illustrateProjection parallelY0' sq

skewSq = mconcat $  [stroke s, stroke (t s) # lc green]
                    ++ zipWith dashedLine facing final
                    ++ zipWith dashedLine (repeat origin) final where
  s = unitSquare # translate (1 ^& 0)
  t = project' 0.000001 facingX
  basePts = concat . pathVertices $ s
  facing = map (project facingX) basePts
  final  = map (project perspectiveX1) basePts

skewCircle = mconcat [stroke c, stroke (project' 0.01 facingX c) # lc green] where
  c = unitCircle # translate (2 ^& 0)

d = [x0, x1, y0Rotated, skewSq, skewCircle]

main = do
    renderSVG "projections.svg" (Dims 500 (300*(realToFrac $ length d))) .
         vcat' (with & sep .~ 0.5) $ d
