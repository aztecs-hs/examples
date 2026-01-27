{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs
import Aztecs.GL.D2
import Aztecs.GLFW
import Control.Monad.IO.Class
import System.IO
import Prelude hiding (lookup)

main :: IO ()
main = runAccess_ $ do
  -- Create the window
  windowEntity <-
    spawn $
      bundle $
        Window
          { windowTitle = "Aztecs GL",
            windowWidth = 800,
            windowHeight = 600
          }

  -- Spawn a red rectangle
  _ <-
    spawn $
      bundle (Rectangle 100 80)
        <> bundle (transform2d {transformTranslation = V2 200 150} :: Transform2D)
        <> bundle (color 1 0 0 1)
        <> bundle (Parent windowEntity)

  -- Spawn a green circle
  _ <-
    spawn $
      bundle (Circle 50 32)
        <> bundle (transform2d {transformTranslation = V2 400 300} :: Transform2D)
        <> bundle (color 0 1 0 1)
        <> bundle (Parent windowEntity)

  -- Spawn a blue triangle
  _ <-
    spawn $
      bundle (Triangle (-40) 40 40 40 0 (-40))
        <> bundle (transform2d {transformTranslation = V2 600 200} :: Transform2D)
        <> bundle (color 0 0 1 1)
        <> bundle (Parent windowEntity)

  -- Spawn a yellow rectangle (rotated)
  _ <-
    spawn $
      bundle (Rectangle 60 60)
        <> bundle (transform2d {transformTranslation = V2 300 450, transformRotation = 45} :: Transform2D)
        <> bundle (color 1 1 0 1)
        <> bundle (Parent windowEntity)

  -- Spawn a magenta circle (scaled)
  _ <-
    spawn $
      bundle (Circle 30 24)
        <> bundle (transform2d {transformTranslation = V2 550 450, transformScale = V2 1.5 1.5} :: Transform2D)
        <> bundle (color 1 0 1 1)
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
