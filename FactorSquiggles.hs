-- Based on http://www.jasondavies.com/primos/ and http://www.polprimos.com/ .

{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude
import           Math.NumberTheory.Primes       (isPrime)

squiggle len d = alignL . hcat . take (ceiling (len / d)) $ cycle [ semi, semi # reflectY ]
  where semi = arc 0 (tau/2 :: Rad) # scale (d/2)

squiggles len = map (squiggle len) [2 .. len]
              # mconcat
              # withEnvelope (mempty :: D R2)
              # atop (strutX len # alignL)

primeMarkers = map (\n -> if isPrime n then square 0.5 else mempty) [0 .. 50]
  # hcat' with { catMethod = Distrib, sep = 1 }

main = defaultMain (squiggles 50)
