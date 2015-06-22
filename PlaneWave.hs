import           Data.Complex
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

type Domain = Complex Double -> AlphaColour Double
type CFunc  = Complex Double -> Complex Double

colors :: [AlphaColour Double]
colors = map opaque [red, yellow, green, cyan, blue, purple]

colorWheel :: Domain
colorWheel c = colors !! (floor (((phase c / tau) + 1/2) * 6) `mod` 6)

lerpColorWheel :: Domain
lerpColorWheel c = blend delta (colors !! ix') (colors !! ix)
  where
    (alpha, delta) = properFraction (((phase c / tau) + 1/2) * 6)
    ix  = alpha `mod` 6
    ix' = (ix + 1) `mod` 6

drawComplex :: Domain -> Int -> Int -> CFunc -> Diagram B
drawComplex dom w h f = rasterDia f' w h
  where
    f' x y = dom (f (toComplex x y))
    toComplex x y = toCoordinate x :+ (- toCoordinate y)
    toCoordinate x = (fromIntegral x / fromIntegral w) * (2*r) - r
    r = 2

i :: Complex Double
i = 0 :+ 1

hWave :: CFunc
hWave z = exp (tau * (0 :+ imagPart z))

type Group = [CFunc]

avgOver :: Group -> CFunc -> CFunc
avgOver grp f z = sum (map (\g -> f (g z)) grp) / fromIntegral (length grp)

zRotate :: Double -> CFunc
zRotate a = (* cis a)

rot3 :: CFunc
rot3 = zRotate (tau/3)

z3 :: Group
z3 = [id, rot3, rot3 . rot3]

dia :: Diagram B
dia = drawComplex lerpColorWheel 400 400 (avgOver z3 hWave)

main = mainWith $ dia
