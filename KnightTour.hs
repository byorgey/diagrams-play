{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.List                      (minimumBy, tails, (\\))
import           Data.Ord                       (comparing)
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Coordinates
import           Diagrams.Prelude

-- Computing a tour.

-- Adapted from http://rosettacode.org/wiki/Knight%27s_tour#Haskell

type Square = (Int, Int)

board :: [Square]
board = [ (x,y) | x <- [0..7], y <- [0..7] ]

knightMoves :: Square -> [Square]
knightMoves (x,y) = filter (flip elem board) jumps
  where jumps = [ (x+i,y+j) | i <- jv, j <- jv, abs i /= abs j ]
        jv    = [1,-1,2,-2]

knightTour :: Square -> [Square]
knightTour sq = knightTour' [sq]
  where
    knightTour' moves@(lastMove:_)
        | null candMoves = reverse moves
        | otherwise = knightTour' $ newSquare : moves
      where newSquare   = minimumBy (comparing (length . findMoves)) candMoves
            candMoves   = findMoves lastMove
            findMoves s = knightMoves s \\ moves

-- Drawing the tour.

squareToPoint :: Square -> P2
squareToPoint (x,y) = fromIntegral x & negate (fromIntegral y)

boardSq c = square 1 # lw 0 # fc c

chessBoard n
  = vcat . map hcat . map (map boardSq)
  . take n . map (take n) . tails
  $ cycle [sandybrown, brown]

knight sq
  = image "white-knight.png" 1 1
  # moveTo (squareToPoint sq)

drawTour tour = tourPoints <> stroke tourPath
  where
    tourPath   = fromVertices . map squareToPoint $ tour
    tourPoints = decoratePath tourPath (repeat dot)
    dot = circle 0.1 # fc black

main = defaultMain $
  mconcat
  [ knight tourStart
  , knight tourEnd
  , drawTour tour
  , chessBoard 8
  ]
  where
    tourStart = (1,3)
    tour      = knightTour tourStart
    tourEnd   = last tour
