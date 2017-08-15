{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

tricurve :: Trail V2 Double
tricurve = mconcat
  [ arc xDir (1/4 @@ turn)
  , arc xDir (1/6 @@ turn) # reverseTrail
  , arc yDir (-1/12 @@ turn)
  ]

main = defaultMain (tricurve # stroke # frame 0.5)
