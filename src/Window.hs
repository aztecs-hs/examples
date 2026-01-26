module Main where

import Aztecs
import Aztecs.GLFW
import Control.Monad.IO.Class
import System.IO
import Prelude hiding (lookup)

main :: IO ()
main = runAccess_ $ do
  e <-
    spawn $
      bundle $
        Window
          { windowTitle = "Aztecs GLFW Window",
            windowWidth = 800,
            windowHeight = 600
          }
  runAccessGLFW $ do
    res <- lookup e
    case res of
      Just keys ->
        if keyJustPressed Key'Escape keys
          then liftIO $ do
            putStrLn "Escape key just pressed, exiting..."
            hFlush stdout
            return True
          else return False
      Nothing -> return False
