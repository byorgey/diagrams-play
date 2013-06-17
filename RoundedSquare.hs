
-- This import is only needed to do actual drawing
import           Diagrams.Backend.Cairo.CmdLine

-- This is the import you need.  To get it, just
--
--   cabal install diagrams-lib
--
import           Diagrams.Prelude

-- Approximate a square with rounded corners by a series of points.
approxRoundedSquare
  :: Int     -- how many points should be used to approximate each rounded corner?
  -> Double  -- the length of one edge of the square
  -> Double  -- the radius of the top left corner
  -> Double  -- top right
  -> Double  -- bottom left
  -> Double  -- bottom right
  -> [P2]
approxRoundedSquare n side tl tr bl br =

  -- approximate the path by a sequence of points
  approxPath n $

  -- use the roundedRect' function to draw a rounded rectangle
  roundedRect' side side with
    { radiusTL = tl
    , radiusTR = tr
    , radiusBL = bl
    , radiusBR = br
    }

-- Approximate a path, replacing curved corners by a list of points.
-- Make the simplifying assumption that all curved edges are
-- quarter-circles traveling counterclockwise.
approxPath :: Int -> Path R2 -> [P2]
approxPath n = concatMap approxSeg . uncurry fixTrail . head . pathTrails
  where
    approxSeg (FLinear p _)    = [p]
    approxSeg (FCubic p _ _ q)
        = iterateN n (rotateAbout ctr (1/(4*fromIntegral n) :: CircleFrac)) p
      where
        ctr = p .+^ project ((q .-. p) # rotateBy (1/8)) (q .-. p)

-- All the stuff below here is not necessary, for testing purposes
-- only

dia = fromVertices $ moveTo (40 & 40) (approxRoundedSquare 4 80 20 20 5 5)

main = defaultMain (dia # centerXY # pad 1.1)

-- vs = [(80.0 & 5.0), (80.0 & 60.0),
--  (78.47759065022574 & 67.6536686473018),
--  (74.14213562373095 & 74.14213562373095),
--  (67.6536686473018 & 78.47759065022574), (60.0 & 80.0),
--  (20.000000000000004 & 80.0),
--  (12.346331352698208 & 78.47759065022574),
--  (5.857864376269049 & 74.14213562373095),
--  (1.5224093497742643 & 67.6536686473018),
--  (0.0 & 60.00000000000001), (0.0 & 5.000000000000007),
--  (0.38060233744356253 & 3.0865828381745573),
--  (1.4644660940672622 & 1.4644660940672694),
--  (3.08658283817455 & 0.38060233744357674),
--  (5.0 & 7.105427357601002e-15), (75.0 & 7.105427357601002e-15),
--  (76.91341716182545 & 0.38060233744357674),
--  (78.53553390593274 & 1.4644660940672765),
--  (79.61939766255642 & 3.0865828381745644),
--  (80.0 & 5.000000000000007)]
