{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Example.Logo
import           Diagrams.Prelude

main = defaultMain $ ico_d # frame 0.5
