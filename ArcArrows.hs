{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

arcArrow :: P2 -> P2 -> Double -> Double -> Diagram Cairo R2
arcArrow p1 p2 ht offs = aDia
  where
    h = abs ht
    straight = h < 0.00001
    v = p2 .-. p1
    d = magnitude (p2 .-. p1)
    th  = acos ((d*d - 4*h*h)/(d*d + 4*h*h))
    r = d/(2*sin th)
    phi | straight = 0
        | otherwise = Rad $ offs/r
    mid | ht >= 0    = tau/4
        | otherwise = 3*tau/4
    st  = mid - Rad th + phi
    end = mid + Rad th - phi
    a | straight
      = hrule (d - 2*offs) # alignL # translateX offs
      | otherwise
      = arc st end
      # scale r
      # translateY ((if ht > 0 then negate else id) (r-h))
      # translateX (d/2)
      # (if ht > 0 then reversePath else id)
    endPt = (\(p,t) -> p .+^ trailOffset t) . head . pathTrails $ a
    hd = triangle 0.1 # rotateBy (-1/4) # alignR # fc black # scaleY 0.7
       # rotate (if ht >= 0 then st - Rad (tau/4) else Rad (3*tau/4) - st)
    aDia
      = ( hd # moveTo endPt
        <>
          a  # stroke
        )
        # rotateBy (direction v)
        # moveTo p1

aa h offs = arcArrow origin (1 & 0) h offs <> p <> p # translateX 1
  where p = circle 0.02 # fc black

dia =
    vcat' with { catMethod = Distrib, sep = 0.3 }
  . map (hcat' with { catMethod = Distrib, sep = 1})
  $ [[aa h offs | offs <- [0,0.05 .. 0.4]] | h <- [0.4, 0.35 .. -0.4]]

main = defaultMain $ dia # centerXY # pad 1.1
