module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.SDL as SDL
import Control.Monad

run :: AccessT IO ()
run = do
  SDL.update
  SDL.draw

app :: AccessT IO ()
app = do
  SDL.setup
  A.spawn_ $ bundle Window {windowTitle = "Aztecs"} <> bundle transform2d
  forever run

main :: IO ()
main = runAccessT_ app
