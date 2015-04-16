{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Diagrams.TwoD.Path.IteratedSubset

main = defaultMain $ illustrateGen 7 dragonGen # frame 0.5

illustrateGen :: Int -> Generator Double -> Diagram B
illustrateGen k g
    = hcat' (with & sep .~ 0.5) [ showGenerator g, iters !! 2, iters !! k ]
    # frame 0.5
  where iters = map strokeLine $ iterGenerator g
