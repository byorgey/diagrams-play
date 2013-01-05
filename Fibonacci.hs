{-# LANGUAGE NoMonomorphismRestriction
           , DeriveFunctor
           , TypeFamilies
           , TupleSections
  #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Control.Arrow
import Control.Newtype

import qualified Data.Map as M
import Data.Maybe

type Partition = [Int]

colors = [black, blue, green, purple]

-- Have to assign names to the diagrams BEFORE they get moved around.
-- Should we make a "Nameable" class and make lists and maps
-- instances?  Note in that case 'named' would NOT commute with
-- mconcat, unless we specified that the name only applies to the
-- FIRST element of the list (which seems a bit odd).

drawPartition = alignL . hcat' with {sep = 0.1} . map drawPart
  where drawPart i = roundedRect (fromIntegral i, 1) 0.2
                   # fc (colors !! i)
                   # lw 0.03

p1 = [1,3,2,2,1]
p2 = [2,1,2,1,3]

keyframe1 = vcat' with {sep = 1} [ [zipWith named ['a'..] $ drawPartition p1]
                                 , [zipWith named ['A'..] $ drawPartition p2]
                                 ]

keyframe2 = hcat' with {sep = 0.1} $ alternate ys xs
  where [xs,ys] = keyframe1

alternate :: [a] -> [a] -> [a]
alternate []     ys = ys
alternate (x:xs) ys = x : alternate ys xs


interpolate :: [Diagram Cairo R2] -> NameMap R2 -> NameMap R2 -> Duration -> Animation Cairo R2
interpolate ds nm1 nm2 dur = mconcat . catMaybes . map animatePiece $ labeled
  where m1 = unpack nm1
        m2 = unpack nm2
        labeled = catMaybes 
                . map strength  
                . map (first getUnique)
                . map ((M.assocs . unpack . names) &&& id) 
                $ ds
        getUnique []       = Nothing
        getUnique [(n,_)]  = Just n
        getUnique _        = Nothing
        strength (fa, b)   = fmap (,b) fa
        animatePiece (n,d) = do
          startLoc <- location <$> (listToMaybe =<< M.lookup n m1)
          endLoc   <- location <$> (listToMaybe =<< M.lookup n m2)
          return $ moveTo <$> ((alerp startLoc endLoc <$> ui) # stretchTo dur) <*> pure d

pieces :: [Diagram Cairo R2]
pieces = concat keyframe1

ns1 = names ((mconcat . concat $ keyframe1) :: D R2)
ns2 = names (keyframe2 :: D R2)

a = interpolate (concat keyframe1) ns1 ns2 1

main = 
  defaultMain ((!!6) . map showOrigin $ pieces)
  -- defaultMain (mconcat . concat $ keyframe1)
  -- defaultMain (activeStart a)
  -- animMain (a <> pure (animRect a # fc white))
