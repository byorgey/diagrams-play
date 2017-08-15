{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

main = defaultMain d

b = hcat' (with & sep .~ 0.2) [circle 1, mempty, mempty, mempty, circle 1]
d = b <> square 1

-- b = mempty ||| circle 1
-- d = b <> square 1
