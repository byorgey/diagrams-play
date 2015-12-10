{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import           Control.Arrow                       ((>>>))
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           System.Random

-- convenient operator for use together with (>>>)
infixr 0 >$>
(>$>) = flip ($)

-- boxes takes as input the grout size, the total height, and a list
-- specifying the number of squares in each column
boxes :: _ -> _ -> [Int] -> Diagram B
boxes grout ht = -- make a column from each #
                 map (column grout ht)      >>>

                 -- put the columns next to each other horizontally with
                 -- 'grout' amount of space between each
                 hsep grout >>>

                 -- center the whole thing
                 centerXY

-- column takes as input the grout size, total height, and the number
-- of squares that should go in the column
column grout ht n = -- make a list of squares
                    replicate n sq             >$>

                    -- lay them out vertically, with 'grout' space in between
                    vsep grout >>>

                    -- align at the top so the columns will line up properly
                    alignT

             -- compute the proper square size
  where sq = square ((ht - (n'-1)*grout) / n') # fc black
        n' = fromIntegral n

main = do
  gen <- getStdGen
  let ns = take 70 $ randomRs (10,50) gen   -- generate some random numbers
  mainWith (pad 1.1 $ boxes 0.3 20 ns)   -- create the diagram
