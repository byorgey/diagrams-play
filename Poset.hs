{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

import           Control.Arrow                  (second)
import qualified Data.Map                       as M
import           Data.Semigroup
import qualified Data.Set                       as S
import           Data.Tuple                     (swap)
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

main = defaultMain mempty

type ID = Int
type Edge = (ID, ID)
type Level = Int

data Poset = Poset (M.Map ID Level) (S.Set Edge)

newtype LevelRange = LR (Option (Max Level), Option (Min Level))
  deriving (Semigroup, Monoid)

lb :: Level -> LevelRange
lb l = LR (Option (Just (Max l)), Option Nothing)

ub :: Level -> LevelRange
ub l = LR (Option Nothing, Option (Just (Min l)))

type LevelMap = M.Map ID LevelRange

mkPoset :: [Edge] -> Poset
mkPoset edges = undefined
  where
    levels = undefined
    mUp    = mkEdgeMap edges
    mDown  = mkEdgeMap (map swap edges)

mkEdgeMap :: [Edge] -> M.Map ID (S.Set ID)
mkEdgeMap = M.fromListWith (S.union) . ((map . second) S.singleton)

mkRanges :: [Edge] -> M.Map ID LevelRange
mkRanges = M.unionsWith (<>) . map edgeRanges

edgeRanges :: LevelMap -> Level -> Level -> Edge -> LevelMap
edgeRanges m lo hi (p,q) = M.fromList [(l, lb l), (h, ub h)]
  where
    lower = m M.! 

{-
posetLevels :: S.Set ID -> M.Map ID (S.Set ID) -> M.Map ID Level
posetLevels ids children
  = fst . head . dropWhile (S.notNull . snd) . iterate nextLevels $ (M.empty, ids)
  where
    nextLevels :: (M.Map ID Level, S.Set ID) -> (M.Map ID Level, S.Set ID)
    nextLevels (m, unlabeled)
      where
-}
