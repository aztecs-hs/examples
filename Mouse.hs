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

update :: AccessT IO ()
update = do
  mouse <- S.single () (Q.fetch @_ @MouseInput)
  liftIO $ print mouse

run :: AccessT IO ()
run = do
  SDL.setup
  A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
  update

main :: IO ()
main = void $ runAccessT run W.empty
