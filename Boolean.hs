{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Trans
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Boolean
import           System.Random

dia :: [P2 Double] -> Diagram B
dia ps = (strokeP $ union Winding (mconcat . map mkC $ ps))
      # lc orange # fc black # lw thick
  where
    mkC pt = circle 1 # moveTo pt

main = do
  ps <- evalRandIO $ do
    xs <- replicateM 100 (getRandomR (-8, 8))
    ys <- replicateM 100 (getRandomR (-8, 8))
    return (zipWith (^&) xs ys)
  mainWith (dia ps # frame 1)
