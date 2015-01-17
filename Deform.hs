{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Lens                   ((^.))
import           Data.Colour.Palette.BrewerSet
import           Data.List                      (permutations)
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Path.Metafont

colors = drop 2 $ brewerSet Purples 7

-- map a vertical line from 0 to h to a trail.
along :: Double -> Located (Trail R2) -> Deformation R2
along h tr = Deformation $ \p ->
  let t       = ((p ^. _y) / h)
      trailPt = tr `atParam` t
      normal  = tr `normalAtParam` t
  in  trailPt .+^ ((p ^. _x) *^ normal)

track :: Located (Trail R2)
track = -- (vrule 3 # reverseTrail <> arc (0 @@ turn) (1/2 @@ turn) # reverseTrail <> vrule 3) `at` origin
  metafont $ origin .- tension 1.1 -. (0 ^& 3) .--. (1 ^& 3) .- tension 1.1 -. endpt (1 ^& 0)

-- dia :: Path R2
-- dia = (hcat' (with & sep .~ 0.1) [vrule 3, vrule 3]) # centerX # alignB # deform' 0.001 (along 3 track)

-- wibble :: Deformation R2
-- wibble = Deformation $ \p ->
--   ((p^._x) + 0.1 * cos ((p ^. _y) * 2 * pi)) ^& (p ^. _y)

drawPerm :: Double -> Double -> [Integer] -> Path R2
drawPerm ht off = mconcat . zipWith drawStrand [0..]
  where
    xPos :: Integer -> Double
    xPos = (off*) . fromIntegral
    drawStrand :: Integer -> Integer -> Path R2
    drawStrand i j = metafont $
      (xPos i ^& 0)
        .- leaving unitY <> arriving unitY -.
      (xPos i ^& (ht * 2 / 5))
        .--.
      (xPos j ^& (ht * 3 / 5))
        .- leaving unitY <> arriving unitY -.
      endpt (xPos j ^& ht)

strokePerm :: Path R2 -> Diagram B R2
strokePerm = mconcat . zipWith (\c lt  -> strokeLocTrail lt # lc c) colors . pathTrails

flower n = permutations [0 .. n-1]
  # map (drawPerm 5 0.3)
  # map centerX
  # map (deform' 0.001 (along 5 track))
  # map centerXY
  # map strokePerm
  # map (rotateBy (-1/4) . translateY (flowerRadius n))
  # zipWith rotateBy [0, 1/factorial n ..]
  # mconcat
  # frame 1

factorial :: Integer -> Double
factorial n = fromIntegral $ product [1 .. n]

flowerRadius 2 = 2
flowerRadius 3 = 4
flowerRadius 4 = 10
-- flowerRadius 5 = 60

main = defaultMain $
   (circle 0.2 # lc (colors !! 0) <> flower 2 # scale 0.5 <> flower 3 <> flower 4 # scale 0.75 {- <> flower 5 # scale 0.16 # lw 0.02 -} ) # lw 0.15 # opacity 0.7
-- drawPerm 5 0.3 [1,2,0] # centerX # deform' 0.001 (along 5 track) # stroke # frame 1
-- (strokeLocTrail track # frame 1)
-- (stroke dia # frame 1)
-- ((circle 3 <> circle 2.9 <> circle 2.8) # deform' 0.0001 wibble # stroke # frame 1)
