{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Control.Monad
import Control.Monad.Random
import Data.List.Split

import Data.Colour
import Data.Colour.SRGB

deriving instance Random CircleFrac

diaR :: RandomGen g => Rand g (Diagram Cairo R2)
diaR = do
  ns <- replicateM 100 (getRandomR (3,5))
  cs <- replicateM 100 (replicateM 3 getRandom)
  ss <- replicateM 100 (getRandomR (0.8, 1.2 :: Double))
  rs <- replicateM 100 (getRandomR (-1/8, 1/8 :: CircleFrac))
  let sqs = map mkReg ns                 -- random # of sides
          # zipWith fc (map toColor cs)  -- random colors
          # zipWith scale ss             -- random scales
          # zipWith rotateBy rs          -- random rotations
      
  return
    . vcat' with {sep = 1, catMethod = Distrib}
    . map (hcat' with {sep = 1, catMethod = Distrib})
    $ chunk 10 sqs
    
mkReg n = polygon with { polyType = PolyRegular n 0.7 }
toColor [r,g,b] = sRGB (0.5*r) g (0.5 + 0.5*b)

main = do
  dia <- evalRandIO diaR
  defaultMain dia