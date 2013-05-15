-- Example for Adrian May, in reply to
-- http://article.gmane.org/gmane.comp.lang.haskell.beginners/11948

{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Maybe                     (fromMaybe)
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

mark = named ()

-- a bunch of stuff, each positioned according to its own sweet logic,
-- some of which are marked
stuff = ( triangle 3 # mark
          ===
          circle 1
        )
        |||
        ( pentagon 2 # mark # translateY 5
          ===
          circle 0.5 # scaleX 3 # mark
        )
        |||
        ( square 2 # mark # translateY 2 )

drawLinesTo x = withNameAll () $ \subs -> applyAll (map drawLineFrom subs)
  where
    drawLineFrom sub = atop (edgePt ~~ xPoint)
      where
        edgePt = fromMaybe origin (maxTraceP (location sub) unitX sub)
        y      = snd (unp2 (location sub))
        xPoint = p2 (x,y)

main = defaultMain (drawLinesTo 13 stuff # centerXY # pad 1.1)
