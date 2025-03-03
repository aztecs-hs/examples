{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import qualified Aztecs.ECS.World as W
import qualified Aztecs.SDL as SDL
import Control.Monad
import Control.Monad.IO.Class

input :: AccessT IO ()
input = do
  kb <- S.single () (Q.fetch @_ @KeyboardInput)
  liftIO $ do
    when (wasKeyPressed KeyW kb) $ print "Onwards!"
    when (wasKeyPressed KeyS kb) $ print "Retreat..."
    when (wasKeyReleased KeyW kb || wasKeyReleased KeyS kb) $ print "Halt!"

update :: AccessT IO ()
update = do
  SDL.update
  input
  SDL.draw
  update

run :: AccessT IO ()
run = do
  SDL.setup
  A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
  update

main :: IO ()
main = void $ runAccessT run W.empty
