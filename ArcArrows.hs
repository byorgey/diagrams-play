{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

arcArrow :: P2 -> P2 -> Double -> Double -> Diagram Cairo R2
arcArrow p1 p2 ht offs = aDia
  where
    h = abs ht
    v = p2 .-. p1
    d = magnitude (p2 .-. p1)
    th  = acos ((d*d - 4*h*h)/(d*d + 4*h*h))
    r = d/(2*sin th)
    phi | ht == 0 = 0
        | otherwise = Rad $ offs/r
    mid | ht >= 0    = tau/4
        | otherwise = 3*tau/4
    st  = mid - Rad th + phi
    end = mid + Rad th - phi
    a | ht == 0
      = hrule (d - 2*offs) # alignL # translateX offs
      | otherwise
      = arc st end
      # scale r
      # translateY ((if ht > 0 then negate else id) (r-h))
      # translateX (d/2)
      # (if ht > 0 then reversePath else id)
    endPt = (\(p,t) -> p .+^ trailOffset t) . head . pathTrails $ a
    hd = triangle 0.1 # rotateBy (-1/4) # alignR # fc black # scaleY 0.7 # rotate (st - Rad (tau/4))
    aDia
      = ( hd # moveTo endPt
        <>
          a  # stroke
        )
        # rotateBy (direction v)
        # moveTo p1

main = defaultMain (square 2 <> square 2.4 <> arcArrow (1&1) (2&2) 0 0.2)
