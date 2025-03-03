{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.World as W
import qualified Aztecs.SDL as SDL
import Aztecs.SDL.Text (Text (..))
import qualified Aztecs.SDL.Text as T
import Control.Monad
import SDL (V2 (..))

setup :: AccessT IO ()
setup = do
  font <- load $ asset "assets/C&C Red Alert [INET].ttf" 24
  A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
  A.spawn_ $
    bundle Camera {cameraViewport = V2 1000 500, cameraScale = 5}
      <> bundle transform2d
  A.spawn_ $
    bundle Text {textFont = font, textContent = "Hello, World!"}
      <> bundle transform2d {transformTranslation = V2 10 10}

run :: AccessT IO ()
run = do
  T.load
  SDL.update
  join T.draw
  SDL.draw
  run

app :: AccessT IO ()
app = do
  SDL.setup
  T.setup
  setup
  run

main :: IO ()
main = void $ runAccessT app W.empty
