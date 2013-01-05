{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import qualified Data.Map as M
import Control.Newtype

type Dia = Diagram Cairo R2

colors = [red, yellow, green, blue, purple, orange, brown, black, white, gray]

drawList :: [Int] -> Dia
drawList = centerX . hcat' with {sep=0.1} . map drawInt
  where
    drawInt :: Int -> Dia
    drawInt i = roundedRect 1 1 0.2 # fc (colors !! i) # named i

dia = drawList [1,3,0,2] # sized (Width 1) # centerXY # pad 1.1

keyframe :: Dia -> Dia -> Animation Cairo R2
keyframe d1 d2 = f <$> ui
  where
    pairs = concat . M.elems $ M.intersectionWith zip (unpack $ subMap d1) (unpack $ subMap d2)
    f t = mconcat . map (\(x1,x2) -> getSub x1 # translate (lerp zeroV (location x2 .-. location x1) (sinify t))) $ pairs

    sinify t = (-cos (t * tau/2) + 1)/2

sjt 0 = [[0]]
sjt n = concat $ zipWith ($) (cycle [map reverse . insert n . reverse, insert n]) ps
  where
    ps = sjt (n-1)
    insert :: Int -> [Int] -> [[Int]]
    insert x [] = [[x]]
    insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)


a = movie . (zipWith keyframe <*> tail) . map drawList . sjt $ 4

r = drawList [0..4] # pad 1.1 # boundingRect

main = animMain (a <> pure r # fc white)