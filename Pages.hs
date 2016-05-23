{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

page :: Double -> Trail V2 Double
page r =
  fromOffsets
  [ ((-8.5 + pi*r) ^& 0)
  , (0             ^& 11)
  , ( (8.5 - pi*r) ^& 0)
  ]
  <>
  arcCW yDir (negated yDir) # scale r # scaleY 0.5
  <>
  fromOffsets
  [ 0 ^& (-11) ]
  <>
  arcCCW (negated yDir) xDir # scale r # scaleY 0.5
  <>
  fromOffsets
  [ 0 ^& 11 ]

labelledPage :: String -> String -> Double -> Diagram B
labelledPage side1 side2 r
  = mconcat
    [ text side1 # moveTo (((-8.5 + pi*r) ^& 11) .+^ off)
    , text side2 # moveTo ((0 ^& (11 - r)) .+^ off)
    , page r # stroke
    ]
    where
      off :: V2 Double
      off = 0.8 ^& (-1)

main = defaultMain (labelledPage "1" "2" 1.5 # frame 1)
