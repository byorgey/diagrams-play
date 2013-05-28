-- Example for Adrian May, in reply to
-- http://article.gmane.org/gmane.comp.lang.haskell.beginners/11948

{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Maybe                     (fromMaybe)
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

-- We can "mark" things just by giving them the name ()
mark = named ()

-- A bunch of stuff, each positioned according to its own sweet logic,
-- some of which are marked.  Note, it's critical that we mark each
-- subdiagram *after* any transformations which we want to affect how
-- its "right edge" is determined (e.g. the scaleX on the circle
-- below), but *before* any transformations which serve to position it
-- in the overall diagram (e.g. the translations of the pentagon and
-- square).
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

-- Draw horizontal lines extending from the right edges of any marked
-- subdiagram to the given x-coordinate.  Extract all the marked
-- subdiagrams using 'withNameAll ()', turn each into a function to
-- draw the required line, and apply all of them.
drawLinesTo x = withNameAll () $ \subs -> applyAll (map drawLineFrom subs)
  where
    drawLineFrom sub = atop (edgePt ~~ xPoint)
      where
        -- Compute the point on the right edge of the subdiagram. This
        -- is a little ugly at the moment; I hope to add combinators
        -- to make this nicer.
        edgePt = fromMaybe origin (maxTraceP (location sub) unitX sub)
        -- Compute the other endpoint of the segment.
        y      = snd (unp2 (location sub))
        xPoint = p2 (x,y)

main = defaultMain (stuff # drawLinesTo 13 # centerXY # pad 1.1)
