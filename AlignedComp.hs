import qualified Control.Lens
import           Data.Maybe                          (fromJust)
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

-- Can do this if we assume diagrams --- then we can use named points.
-- But we can't add this as another option to CatOpts, because it
-- isn't generic enough to be part of 'cat'.

composeAligned
  :: (Monoid' m, Floating n, Ord n, Metric v)
  => (QDiagram b v n m -> QDiagram b v n m)
  -> ([QDiagram b v n m] -> QDiagram b v n m)
  ->  [QDiagram b v n m] -> QDiagram b v n m
composeAligned _ combine [] = combine []
composeAligned alignment combine (d:ds) = (combine $ map alignment (d:ds)) # moveOriginTo l
  where
    mss = alignment ((() .>> d) # named ()) ^. subMap . _Wrapped . Control.Lens.at (toName ())
    l   = location . head . fromJust $ mss

d1, d2 :: Diagram B
d1 = square 1
d2 = circle 1
d3 = triangle 2

dia = ((mconcat # composeAligned alignT) [d1,d2,d3]) # showOrigin

main = mainWith (dia # frame 0.5)
