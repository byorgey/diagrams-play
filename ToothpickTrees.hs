{- http://bit-player.org/2013/joshua-trees-and-toothpicks -}

import           Control.Arrow (second)
import           Control.Lens  ((&), _1, _2, (%~))
import           Data.List     (group, sort)
import qualified Data.Set as S
import           Diagrams.Prelude hiding ((&), End)

data Dir = H | V
  deriving (Eq, Show, Ord)

flipDir H = V
flipDir V = H

type Loc = (Int, Int)
data End = End { endLoc :: Loc, endDir :: Dir }
  deriving (Eq, Show, Ord)
data Toothpick = Toothpick Loc Dir (Colour Double)
  deriving (Show)

instance Eq Toothpick where
  (Toothpick l1 d1 _) == (Toothpick l2 d2 _) = l1 == l2 && d1 == d2

instance Ord Toothpick where
  compare (Toothpick l1 d1 _) (Toothpick l2 d2 _) = compare l1 l2 `mappend` compare d1 d2

data ToothpickTree = TT { ttGen  :: Int
                        , ttToothpicks :: S.Set Toothpick
                        , ttLocs :: S.Set Loc
                        , ttEnds :: [End]
                        }
  deriving (Show)

toothpickEnds (Toothpick l dir _)
  = [End (l & x %~ pred) dir, End (l & x %~ succ) dir]
  where
    x = case dir of
          H -> _1
          V -> _2

grow :: Colour Double -> End -> (Toothpick, [End])
grow c (End l dir) = ( t, toothpickEnds t )
  where
    dir' = flipDir dir
    t = Toothpick l dir' c

colors = [red, blue, green]

growTree (TT gen toothpicks locs tips)
    = (TT (gen + 1)
          (toothpicks `S.union` S.fromList newTPs)
          (locs `S.union` (S.fromList $ map endLoc newEnds))
          newTips
      )
  where
    c                 = colors !! (gen `mod` length colors)
    (newTPs, newEnds) = second concat . unzip . map (grow c) $ tips
    newEnds'          = map head . filter (null . drop 1) . group . sort $ newEnds
    newTips           = filter ((`S.notMember` locs) . endLoc) newEnds'

initToothpick = Toothpick (0,0) V (colors !! 0)
initTree = TT 0
              (S.singleton initToothpick)
              (S.fromList $ (0,0) : map endLoc es)
              es
  where es = toothpickEnds initToothpick

tt n = iterate growTree initTree !! n

--------------------------------------------------

locToP2 (x,y) = p2 (fromIntegral x, fromIntegral y)

drawToothpick (Toothpick loc dir c)
  = (case dir of {H -> hrule; V -> vrule}) 2 # moveTo (locToP2 loc) # lc c

drawToothpicks = sized (Width 4) . mconcat . map drawToothpick . S.toList

drawTT = drawToothpicks . ttToothpicks . tt