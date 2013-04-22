{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.Map                       as M
import qualified          Data.Set as S
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

main = defaultMain mempty

type ID = Int
type Edge = (ID, ID)
type Level = Double

data Poset a = Poset (M.Map ID (a, Level)) (S.Set Edge)

posetLevels :: S.Set ID -> M.Map ID (S.Set ID) -> M.Map ID Level
posetLevels ids children
  = fst . head . dropWhile (S.notNull . snd) . iterate nextLevels $ (M.empty, ids)
  where
    nextLevels :: (M.Map ID Level, S.Set ID) -> (M.Map ID Level, S.Set ID)
    nextLevels (m, unlabeled)
      where
