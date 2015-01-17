{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

rightOf :: IsName n => Diagram B R2 -> n -> Diagram B R2 -> Diagram B R2
rightOf d n = withName n $ \sub -> atop (juxtapose unitX (getSub sub) (d # moveTo (location sub)))

squares = vcat' (with & sep .~ 0.5) (map mkBox [0 :: Int .. 6])
  where
    mkBox i = (square 1 <> text (show i)) # named i

dia = squares
    # (circle 0.5) `rightOf` (2 :: Int)

main = defaultMain (dia # centerXY # frame 1)
