{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmF she #-}

import Data.Maybe
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

import Data.Colour

type A2 = Animation Cairo R2

spot  = circle 0.1 # fc red # lw 0
-- wheel = vrule 1 # alignT # lw 0.05 <> circle 1 # fc green
wheel = mconcat
      $ zipWith fc (cycle [blue, lightblue])
      $ zipWith (wedge 1) angles (tail angles)
  where
    angles = [0, tau/10 .. tau] :: [Rad]

spotRad = 0.3

rim = circle (1 + spotRad) # fc yellow

wheelAssembly = mconcat
  [ spot
    # named "spot"
    # withEnvelope (mempty :: D R2)
    # translateY (-spotRad)
  , (wheel <> rim # withEnvelope (mempty :: D R2))
    # alignB
  ]
  # centerXY

theta :: Active Double
theta = ui # scale tau

cycloid :: A2
cycloid =
    (| translateX theta
        (| rotate (| (Rad . negate) theta |)
            ~wheelAssembly
         |)
     |) # setEra (mkEra 0 2)
    =^=
    pure ground # alignL

infixl 6 =^=
(=^=) d1 d2 = juxtapose unit_Y d1 d2 <> d1

ground = hrule (tau * 2) # lw 0.05

anim = stretch 3 . mconcat $
  [ cycloid
  , pure (cubicSpline False pts)
  , pure (animRect cycloid # scaleY 2) # fc white # lw 0
  ]

pts = map (location . head . fromJust . lookupSub "spot" . names) (simulate 30 cycloid)

main = animMain anim >> print pts
