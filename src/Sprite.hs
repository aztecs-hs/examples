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
    spawn . bundle $
      Window
        { windowTitle = "Aztecs GL",
          windowWidth = 800,
          windowHeight = 600
        }

  -- Spawn a sprite
  img <- liftIO $ loadImage "assets/logo.jpg"
  imgEntity <- spawn $ bundle img
  _ <-
    spawn $
      bundle (sprite imgEntity)
        <> bundle (transform2d {transformTranslation = V2 200 150} :: Transform2D)
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
