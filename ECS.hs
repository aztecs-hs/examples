{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

move :: (Monad m) => QueryT m Position
move = Q.fetch & Q.adjust (\(Velocity v) (Position p) -> Position $ p + v)

run :: AccessT IO ()
run = do
  positions <- S.map move
  liftIO $ print positions

app :: AccessT IO ()
app = do
  A.spawn_ $ bundle (Position 0) <> bundle (Velocity 1)
  forever run

main :: IO ()
main = runAccessT_ app
