{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.Cairo.CmdLine
import qualified Diagrams.Prelude as D
import Diagrams.Prelude hiding (sample)

import Control.Monad
import Data.Random

pointDistrib :: RVar P2
pointDistrib = do
  theta <- uniform 0 tau
  r     <- normal 4 1
  return $ (cos theta & sin theta) # scale r

ring :: Diagram Cairo R2
ring = stroke $ circle 5 <> circle 3 # reversePath

colorFun :: P2 -> RVar (AlphaColour Double)
colorFun p@(coords -> (x :& y)) = do
  let m = (magnitude (p .-. origin) - 2) / 4
  t <- normal m 0.1
  return (blend t white blue `withOpacity` 0.8)

main = do
  pts <- replicateM 1000 (sample pointDistrib)
--  let pts' = filter (getAny . D.sample ring) pts
  pts' <- mapM (\p -> sample (colorFun p) >>= \c -> return (p, circle 0.1 # fcA c # lw 0)) pts
  defaultMain (position pts')