{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Arrow
import Debug.Trace
import Data.List.Split
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Control.Lens ((^.))

-- import Diagrams.TwoD.Path.IteratedSubset

main = defaultMain $ (dia <> (square 1 # translateY 0.5)) # frame 0.5

  -- $ (showOrigin . uncurry atop . (stroke *** stroke) $ arrowheadHalfDart (2/5 @@ turn :: Angle Double) 1 0.3)

dia = arrowV' (with & arrowHead .~ dart & shaftStyle %~ lc blue) unitX # lw veryThick
