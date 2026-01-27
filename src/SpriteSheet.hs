{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs
import Aztecs.GL.D2
import Aztecs.GLFW
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Time.Clock
import System.IO
import Prelude hiding (lookup)

-- Game state
data GameState = GameState
  { lastTime :: !UTCTime,
    elapsedTime :: !Float
  }
  deriving (Show)

-- Game monad
type GameM = StateT GameState IO

-- Animated component
data Animated = Animated {animatedFrame :: Int, animatedFrameCount :: Int}
  deriving (Show)

instance (Monad m) => Component m Animated

-- Animate all animated sprites
animate :: Access GameM ()
animate = do
  _ <- system . runQuery . queryMapWith go' $ queryMap go
  return ()
  where
    go animated =
      let newFrame = (animatedFrame animated + 1) `mod` animatedFrameCount animated
       in animated {animatedFrame = newFrame}
    go' animated spriteSheet = spriteSheet {spriteSheetFrameIndex = V2 (animatedFrame animated) 0}

main :: IO ()
main = do
  now <- getCurrentTime
  evalStateT (runAccess_ run) GameState {lastTime = now, elapsedTime = 0}

run :: Access GameM ()
run = do
  -- Create the window
  windowEntity <-
    spawn . bundle $
      Window
        { windowTitle = "Aztecs GL - Spritesheet Animation",
          windowWidth = 800,
          windowHeight = 600
        }

  -- Spawn an animated sprite
  img <- liftIO $ loadImage "assets/IDLE.png"
  imgEntity <- spawn $ bundle img
  _ <-
    spawn $
      bundle (sprite imgEntity)
        <> bundle
          SpriteSheet
            { spriteSheetFrameIndex = V2 0 0,
              spriteSheetFrameSize = V2 20 34,
              spriteSheetFrameOffset = V2 36 46,
              spriteSheetFrameGap = V2 76 0
            }
        <> bundle Animated {animatedFrame = 0, animatedFrameCount = 10}
        <> bundle (transform2d {transformScale = V2 5 5, transformTranslation = V2 200 150} :: Transform2D)
        <> bundle (Parent windowEntity)

  -- Run
  runAccessGLFW $ loop windowEntity

loop :: EntityID -> Access GameM Bool
loop windowEntity = do
  -- Calculate elapsed time
  now <- liftIO getCurrentTime
  lt <- lift $ gets lastTime
  let dt = realToFrac (diffUTCTime now lt) :: Float
  lift . modify' $ \s -> s {lastTime = now}

  -- Animate
  accum <- lift $ gets elapsedTime
  let frameDuration = 0.1
      newAccum = accum + dt
  if newAccum >= frameDuration
    then do
      lift . modify' $ \s -> s {elapsedTime = newAccum - frameDuration}
      animate
    else lift . modify' $ \s -> s {elapsedTime = newAccum}

  render

  -- Check for exit
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
