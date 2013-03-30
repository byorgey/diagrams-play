{-# LANGUAGE TypeFamilies #-}
module RoundedPaths 
    ( offsetPath
    , fromFixed
--    , roundedCornerPath
    ) where

import           Diagrams.Prelude hiding (arc')

offsetPath :: Double -> Path R2 -> Path R2
offsetPath r = Path . map (fromFixed . expandFixedSegments r) . fixPath

expandFixedSegments :: Double -> [FixedSegment R2] -> [FixedSegment R2]
expandFixedSegments _ [] = []
expandFixedSegments r fs = caps r s e (f r) (f $ -r)
  where
    f r = joinSegments r ends . map (offsetFixedSegment r) $ fs
    ends = tail . map fstart $ fs
    
    s = fstart (head fs)
    e = fend (last fs)
    
-- Here we do not have a good option for `FCubic`.  At the least
-- We would want to result in several segments if we attempt a 
-- real approximation.  For example, for a given `f@(FCubic ...)`
-- if first subdividing `f` into `n` segments and applying the 
-- transformation here we approach the correct solution as `n`
-- approaches infinity.
offsetFixedSegment :: Double -> FixedSegment R2 -> FixedSegment R2
offsetFixedSegment r f@(FCubic a b c d) = 
    FCubic (a .+^ va) (b .+^ va) (c .+^ vc) (d .+^ vc)
  where va = r *^ vperp (b .-. a)
        vc = r *^ vperp (d .-. c)
offsetFixedSegment r (FLinear a b) = FLinear (a .+^ v) (b .+^ v)
  where v = r *^ vperp (b .-. a)
  
-- Perpendicular to the right.  We want to be on the outside of a 
-- counter-clockwise closed simple path.
vperp :: R2 -> R2
vperp v = rotateBy (-1/4) (normalized v)

caps :: Double -> P2 -> P2 -> [FixedSegment R2] -> [FixedSegment R2] -> [FixedSegment R2]
caps r _ _ _  [] = []
caps r s e fs bs = concat [cap r s sa sb, fs, cap r e ea eb, reverse . map rev $ bs]
  where
    sa = fstart (head bs)
    sb = fstart (head fs)
    
    ea = fend (last fs)
    eb = fend (last bs)
    
    rev (FLinear a b)     = FLinear b a
    rev (FCubic  a b c d) = FCubic  d c b a

-- Caps go on the end of an expanded path and should represent the 
-- style of stroke being applied.
cap, capCut, capRound :: Double -> P2 -> P2 -> P2 -> [FixedSegment R2]

capCut r c a b = [FLinear a b]

capRound r c a b = fixedArc r c a b

cap = capRound
-- intersect
-- arc

joinSegments :: Double -> [Point R2] -> [FixedSegment R2] -> [FixedSegment R2]
joinSegments r es [] = []

joinSegments r es fs@(f:_) = f : concat 
    [joinSegment r e a b ++ [b] | (e,(a,b)) <- zip es . (zip <*> tail) $ fs]

-- Ways to join two segments:
joinSegment, joinSegmentCut, joinSegmentClip, joinSegmentArc :: Double -> P2 -> FixedSegment R2 -> FixedSegment R2 -> [FixedSegment R2]
-- Join with segments going back to the original corner.
joinSegmentCut r e a b = [FLinear (fend a) e, FLinear e (fstart b)]

-- This option works for any corner, just connecting the
-- offset segments.  On an inside corner this creates negative
-- space for even-odd fill.  Here is where we would want to
-- use an arc or something else in the future.
joinSegmentClip _ _ a b = [FLinear (fend a) (fstart b)]

-- Since we have expanded with a consistent radius we can fit a radius arc
-- here.  We don't really want to do this on an inside corner, but no harm
-- is done given winding fill.
joinSegmentArc r e a b = fixedArc r e (fend a) (fstart b)
-- joinSegmentIntersect

joinSegment = joinSegmentArc

arc' a b
  | a < 0     = arc' (a + convertAngle (1 :: CircleFrac)) (b + convertAngle (1 :: CircleFrac))
  | a <= b    = arc a b
  | otherwise = arc' a (b + convertAngle (1 :: CircleFrac))
  
arcCW' a b = reversePath (arc' b a)
  
arcV u v = arc' (direction u) (direction v :: CircleFrac)

arcVCW u v = arcCW' (direction u) (direction v :: CircleFrac)

-- Negative r means CW
fixedArc :: Double -> P2 -> P2 -> P2 -> [FixedSegment R2]
fixedArc r c a b = f . head . fixPath . moveTo c $ fs
  where
    fs | r < 0     = scale (-r) $ arcVCW (a .-. c) (b .-. c)
       | otherwise = scale r    $ arcV   (a .-. c) (b .-. c)
  
    f fs
      |  fstart (head fs) =~= a
      && fend   (last fs) =~= b = fs
      |  otherwise              = error ("fixedArc: " ++ show (r,c,a,b,fs))

    a =~= b = magnitude (a .-. b) < 0.01

fstart (FLinear a _) = a
fstart (FCubic a _ _ _) = a
fend (FLinear _ b) = b
fend (FCubic _ _ _ d) = d

fromFixed :: [FixedSegment R2] -> (Point R2, Trail R2)
fromFixed fs = fromFixed' . map (uncurry checkEnds) . (zip <*> tail) $ fs
  where
    checkEnds a b
      | fend a =~= fstart b = a
      | otherwise           = error ("fromFixed: " ++ show (a,b))

    a =~= b = magnitude (a .-. b) < 0.01

fromFixed' :: [FixedSegment R2] -> (Point R2, Trail R2)
fromFixed' [] = (p2 zeroV, Trail [] False) -- ???
fromFixed' (s:ss) = (a, Trail (b : map (snd . rel) ss) True)
  where
    (a, b) = rel s
  
    rel (FLinear a b) = (a, Linear $ b .-. a)
    rel (FCubic a b c d) = (a, Cubic (b .-. a) (c .-. a) (d .-. a))

-- roundedCornerPath
