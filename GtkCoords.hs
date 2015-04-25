{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (liftIO)
import           Data.IORef
import           Data.List                       (intercalate)
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude
import           Graphics.UI.Gtk

main :: IO ()
main = do
  -- Ordinary Gtk setup.
  void initGUI
  w <- windowNew
  da <- drawingAreaNew
  w `containerAdd` da
  void $ w `on` deleteEvent $ liftIO mainQuit >> return True

  -- Make an IORef to hold the transformation from window to diagram
  -- coordinates.
  gtk2DiaRef <- (newIORef mempty :: IO (IORef (T2 Double)))

  -- Render the diagram on the drawing area.
  void $ da `on` exposeEvent $ liftIO $ do
    dw <- widgetGetDrawWindow da

    -- renderDiaT returns both a rendering result as well as the
    -- transformation from diagram to output coordinates.
    let (dia2gtk, (_,r)) = renderDiaT Cairo
                             (CairoOptions "" (mkWidth 250) PNG False)
                             prettyHouse

    -- store the inverse of the diagram -> window coordinate transformation
    -- for later use in interpreting mouse clicks
    writeIORef gtk2DiaRef (inv dia2gtk)

    renderWithDrawable dw r
    return True

  -- When the mouse moves, show the coordinates and the objects under
  -- the pointer.
  void $ da `on` motionNotifyEvent $ do
    (x,y) <- eventCoordinates

    -- transform the mouse click back into diagram coordinates.
    gtk2Dia <- liftIO $ readIORef gtk2DiaRef
    let pt' = transform gtk2Dia (p2 (x,y))

    liftIO $ do
      putStrLn $ show (x,y) ++ ": "
                   ++ intercalate " " (sample prettyHouse pt')
      return True

  -- Run the Gtk main loop.
  da `widgetAddEvents` [PointerMotionMask]
  widgetShowAll w
  mainGUI

-- The diagram to be drawn, with features tagged by strings.
prettyHouse :: QDiagram Cairo V2 Double [String]
prettyHouse = house
  where
    roof    = triangle 1   # scaleToY 0.75 # centerY # fc blue
    door    = rect 0.2 0.4 # fc red
    handle  = circle 0.02  # fc black
    wall    = square 1     # fc yellow
    chimney = fromOffsets [0 ^& 0.25, 0.1 ^& 0, 0 ^& (-0.4)]
            # closeTrail # strokeT # fc green
            # centerX
            # named "chimney"
    smoke = mconcat
      [ circle 0.05 # translate v
      | v <- [ zero, 0.05 ^& 0.15 ]
      ]
      # fc grey
    house = vcat
      [ mconcat
        [ roof    # snugR                  # value ["roof"]
        , chimney # snugL                  # value ["chimney"]
        ]
        # centerX
      , mconcat
        [ handle  # translate (0.05 ^& 0.2) # value ["handle"]
        , door    # alignB                  # value ["door"]
        , wall    # alignB                  # value ["wall"]
        ]
      ]
      # withName "chimney" (\chim ->
          atop (smoke # moveTo (location chim) # translateY 0.4
                      # value ["smoke"]
               )
        )
