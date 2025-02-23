{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import qualified Aztecs.SDL as SDL
import Aztecs.SDL.Image (Image (..))
import qualified Aztecs.SDL.Image as IMG
import Control.Arrow
import Control.DeepSeq
import GHC.Generics
import SDL  ( V2 (..),)

data PlayerKind = LeftPlayer | RightPlayer
  deriving (Show, Eq, Generic, NFData)

data Player = Player
  { playerKind :: PlayerKind,
    playerPosition :: Float
  }
  deriving (Show, Eq, Generic, NFData)

instance Component Player

setup :: Schedule IO () ()
setup = proc () -> do
  ballTexture <- system . load $ asset "assets/ball.png" () -< ()
  paddleTexture <- system . load $ asset "assets/paddle.png" () -< ()

  access
    ( \(ballTexture, paddleTexture) -> do
        A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
        A.spawn_ $ bundle Camera {cameraViewport = V2 1000 500, cameraScale = 5} <> bundle transform2d
        A.spawn_ $
          bundle Player {playerKind = LeftPlayer, playerPosition = 0}
            <> bundle transform2d
            <> bundle Image {imageTexture = paddleTexture}
        A.spawn_ $
          bundle Player {playerKind = RightPlayer, playerPosition = 0}
            <> bundle transform2d {transformTranslation = V2 50 0}
            <> bundle Image {imageTexture = paddleTexture}
        A.spawn_ $
          bundle Image {imageTexture = ballTexture}
            <> bundle transform2d {transformTranslation = V2 10 10}
    )
    -<
      (ballTexture, paddleTexture)

update :: System () ()
update = proc () -> do
  kb <- S.single Q.fetch -< ()
  let delta = 0.01
      makeDelta keyUp keyDown =
        if isKeyPressed keyUp kb then -delta else if isKeyPressed keyDown kb then delta else 0
      leftDelta = makeDelta KeyW KeyS
      rightDelta = makeDelta KeyUp KeyDown
  S.map
    ( proc (leftDelta, rightDelta) -> do
        player <- Q.fetch -< ()
        transform <- Q.fetch @_ @Transform2D -< ()
        let d = case playerKind player of
              LeftPlayer -> leftDelta
              RightPlayer -> rightDelta
            V2 x _ = transformTranslation transform
            y = playerPosition player + d
        Q.set -< transform {transformTranslation = V2 x $ round y}
        Q.set -< player {playerPosition = y}
    )
    -<
      (leftDelta, rightDelta)
  returnA -< ()

game :: Schedule IO () ()
game =
  SDL.setup
    >>> system IMG.setup
    >>> setup
    >>> forever_
      ( IMG.load
          >>> SDL.update
          >>> system update
          >>> system IMG.draw
          >>> SDL.draw
      )

main :: IO ()
main = runSchedule_ game
