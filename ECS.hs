{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import qualified Aztecs.ECS.World as W
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

move :: (ArrowQuery m q) => q () Position
move = proc () -> do
  Velocity v <- Q.fetch -< ()
  Position p <- Q.fetch -< ()
  Q.set -< Position $ p + v

run :: (ArrowQuery m q, MonadSystem q s, MonadIO s) => s ()
run = do
  positions <- S.map () move
  liftIO $ print positions
  run

app :: AccessT IO ()
app = do
  A.spawn_ $ bundle (Position 0) <> bundle (Velocity 1)
  run

main :: IO ()
main = void $ runAccessT app W.empty
