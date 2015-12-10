import           Data.Tree
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

l c = Node c []

t :: Int -> Tree (Int,Int)
t = t' (1,1)
  where
    t' p 0 = l p
    t' (a,b) n = Node (a,b) [t' (a,a+b) (n-1), t' (a+b,b) (n-1)]

-- t = Node 'A' [l 'B', Node 'C'[ l 'X', l 'Y', l 'Z'], l 'D', l 'E', l 'F', l 'G', l 'H', l 'I', l 'J', l 'K', l 'L', l 'M' ]

example :: Diagram B
example =
   renderTree (\(a,b) -> (text (show a ++ "/" ++ show b) # fontSizeL 0.6
                            <> circle 1 # fc white))
             (~~) (radialLayout (t 4))
   # centerXY # pad 1.1

main = mainWith $ example # frame 1
