{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

colors = [blend 0.3 grey white, blue, red, yellow, green, purple, orange, brown, black, pink]

nextrow :: Integer -> [Integer] -> [Integer]
nextrow d xs = zipWith (\x y -> (x + y) `mod` d) (0:xs) (xs ++ [0])

pascal d = iterate (nextrow d) [1]

diag = fromOffsets (repeat (2 *^ unitX)) # rotateBy (-1/3)

drawPascal d rows = decorateTrail diag (map (hcat . map drawCell) . take rows $ pascal d)

drawCell n = circle 1 # fc (colors !! fromIntegral n) # lw 0

main = defaultMain (drawPascal 10 100)