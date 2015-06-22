import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Diagrams.Transform.ScaleInv
import           Diagrams.TwoD.Text

theSquare = square 4 === strutY 0.5
theText = scaleInv (juxtapose unit_Y theSquare (text "hello")) unitX

(theSquare',theText') = (theSquare, theText) # scaleX 2

dia1, dia2 :: Diagram B
dia1 = theSquare <> (theText ^. scaleInvObj)  -- untransformed, for comparison
dia2 = theSquare' <> (theText' ^. scaleInvObj)

main = mainWith $ hsep 1 [dia1, dia2] # frame 1
