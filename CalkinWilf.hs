{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Arrow                      ((&&&))
import           Data.Function                      (on)
import           Data.List
import           Data.Maybe
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.CirclePacking
import           Diagrams.TwoD.Layout.Tree

data CWInt = Copy Integer | Add Integer Integer

toInt :: CWInt -> Integer
toInt (Copy i) = i
toInt (Add i j) = i + j

instance Num CWInt where
  fromInteger = Copy
  x + y       = Add (toInt x) (toInt y)

instance Eq CWInt where
  (==) = (==) `on` toInt

instance Ord CWInt where
  compare = compare `on` toInt

copy = Copy . toInt

cwTree :: BTree (CWInt, CWInt)
cwTree = cwTree' (1,1)
  where
    cwTree' (a,b) = BNode (a,b) (cwTree' (copy a, a+b)) (cwTree' (a+b, copy b))

cut :: Int -> BTree a -> BTree a
cut 0 _             = Empty
cut _ Empty         = Empty
cut n (BNode a l r) = BNode a (cut (n-1) l) (cut (n-1) r)

t = sized (Width 10)
  . renderTree' fst cwEdge
  . fromJust
  . symmLayoutBin' with { slWidth  = fromMaybe (0,0) . extentX . fst
                        , slHSep   = 0
                        , slVSep   = 3
                        }
  . fmap (cwNode &&& id)
  . cut 6
  $ cwTree

cwEdge (_,p) ((_,(a,b)),q)
    = (   (redP ~~ redC) # lc red
       <> (blueP ~~ blueC) # lc blue
       <> (if a < b
              then (redP ~~ blueC) # lc red
              else (blueP ~~ redC) # lc blue
          )
      )
      # lw 0.02
  where
    [[redP,blueP],[redC,blueC]] = [ [ pt # translateX x | x <- [-0.5, 0.5] ] | pt <- [p,q] ]

cwNode (a,b)
  = (cwBlob a (blend 0.3 white red) # adj ||| cwBlob b (blend 0.3 white blue) # adj) # centerX
  where
    adj | a > b     = reflectX
        | otherwise = id

cwBlob n c   -- Fix this to take CW-int into account
  = atop (circle 0.5)
  . beneath (circle 0.5 # lw 0 # fc white)
  . (\cp -> scale (0.5 / approxRadius 12 cp) cp)
  . createCirclePacking (const 1) (\c -> circle 1 # lw 0 # fc c)
  $ case n of
      Copy i  -> genericReplicate i c
      Add i j -> genericReplicate i red ++ genericReplicate j blue


main = defaultMain (t # centerXY # pad 1.1)
