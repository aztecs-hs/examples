{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs
import Aztecs.GL.D2
import Aztecs.GL.Text
import Aztecs.GLFW
import Control.Monad.IO.Class
import System.IO
import Prelude hiding (lookup)

main :: IO ()
main = runAccess_ $ do
  -- Create the window
  windowEntity <-
    spawn . bundle $
      Window
        { windowTitle = "Aztecs GL",
          windowWidth = 800,
          windowHeight = 600
        }

  -- Spawn text labels
  font <- spawn . bundle $ Font "assets/Montserrat-VariableFont_wght.ttf"
  _ <-
    spawn $
      bundle (Label "Hello from" font defaultLabelStyle {labelFontSize = 48, labelItalic = True})
        <> bundle (transform2d {transformTranslation = V2 200 150} :: Transform2D)
        <> bundle (Parent windowEntity)
  _ <-
    spawn $
      bundle (Label "Aztecs" font defaultLabelStyle {labelFontSize = 48, labelFontWeight = 900, labelColor = V4 1 0 0 1})
        <> bundle (transform2d {transformTranslation = V2 200 100} :: Transform2D)
        <> bundle (Parent windowEntity)

  -- Run
  runAccessGLFW $ do
    render
    mKeys <- lookup @_ @Keys windowEntity
    case mKeys of
      Just keys ->
        if keyJustPressed Key'Escape keys
          then do
            liftIO $ do
              putStrLn "Escape key just pressed, exiting..."
              hFlush stdout
            return True
          else return False
      Nothing -> return False
