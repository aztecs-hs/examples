module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.SDL as SDL
import Aztecs.SDL.Image (Image (..))
import qualified Aztecs.SDL.Image as IMG
import Control.Monad
import SDL (V2 (..))

setup :: AccessT IO ()
setup = do
  texture <- load $ asset "assets/example.png" ()
  A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
  A.spawn_ $
    bundle Camera {cameraViewport = V2 1000 500, cameraScale = 5}
      <> bundle transform2d
  A.spawn_ $
    bundle Image {imageTexture = texture}
      <> bundle transform2d {transformTranslation = V2 10 10}

run :: AccessT IO ()
run = do
  IMG.load
  SDL.update
  join IMG.draw
  SDL.draw

app :: AccessT IO ()
app = do
  SDL.setup
  IMG.setup
  setup
  forever run

main :: IO ()
main = runAccessT_ app
