-- Inspired by http://www.jasondavies.com/set-partitions/

{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import RoundedPaths

import Control.Arrow
import Data.List

-- powerSet S generates the power set of S, with each subset paired
--   with its complement.
--   e.g. powerSet [1,2] = [([1,2],[]),([1],[2]),([2],[1]),([],[1,2])].
powerSet :: [a]   -> [([a],[a])]
powerSet []     = [([],[])]
powerSet (x:xs) = map (first (x:)) psRest ++ map (second (x:)) psRest
  where
    psRest = powerSet xs

type Partition a = [[a]]

-- partitions S generates a list of partitions of S.
-- e.g. partitions [1,2,3] = [[[1,2,3]],[[1,2],[3]],[[1,3],[2]],[[1],[2,3]],[[1],[2],[3]]].
partitions :: [a] -> [Partition a]
partitions []     = return []
partitions (x:xs) = do
  (sub,rest) <- powerSet xs
  ps         <- partitions rest
  return ((x:sub) : ps)

drawPartition :: Partition Int -> Diagram Cairo R2
drawPartition p = pts # applyAll (zipWith drawPart colors p)
  where
    pts = decorateTrail tr (zipWith named [0::Int ..] $ repeat dot)
    tr  = polygon with {polyType = PolyRegular n 1, polyOrient = NoOrient}
        # rotateBy (1/4)
    n   = length (concat p)
    dot = circle 0.2 # fc black

colors = [blue, green, yellow, red]

drawPart :: Colour Double -> [Int] -> (Diagram Cairo R2 -> Diagram Cairo R2)
drawPart c p = withNames p $
    beneath
  . fcA (withOpacity c 0.5)
  . lw 0
  . stroke
  . offsetPath 0.3
  . close
  . fromVertices
  . map location

main = defaultMain (drawPartition [[1,3,4],[0,2],[7,8,9,5],[6,10]] # centerXY # pad 1.2)