{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs
import Aztecs.GL
import Aztecs.GLFW
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Time.Clock
import System.IO
import Prelude hiding (lookup)

windowW, windowH :: Int
windowW = 800
windowH = 600

paddleW, paddleH :: Float
paddleW = 15
paddleH = 80

ballR :: Float
ballR = 10

paddleSpeed :: Float
paddleSpeed = 400

ballInitSpeedX, ballInitSpeedY :: Float
ballInitSpeedX = 300
ballInitSpeedY = 200

paddleMargin :: Float
paddleMargin = 40

newtype GameState = GameState {gsLastTime :: UTCTime}
  deriving (Show)

type GameM = StateT GameState IO

-- Paddle component
newtype Paddle = Paddle {paddleScore :: Int} deriving (Show)

instance (Monad m) => Component m Paddle

-- Ball component
data Ball = Ball
  { ballVelX :: !Float,
    ballVelY :: !Float
  }
  deriving (Show)

instance (Monad m) => Component m Ball

main :: IO ()
main = do
  now <- getCurrentTime
  let initialState = GameState {gsLastTime = now}
  (_, _) <- runStateT (runAccess_ setupAndRun) initialState
  putStrLn "Game ended!"

setupAndRun :: Access GameM ()
setupAndRun = do
  -- Spawn the window
  windowEntity <-
    spawn $
      bundle $
        Window
          { windowTitle = "Pong - W/S & Up/Down - ESC to quit",
            windowWidth = windowW,
            windowHeight = windowH
          }

  let centerY = fromIntegral windowH / 2 :: Float
      centerX = fromIntegral windowW / 2 :: Float

  -- Spawn center line
  _ <-
    spawn $
      bundle (Rectangle 4 (fromIntegral windowH))
        <> bundle (transform2d {transformTranslation = V2 (round centerX) (round centerY)} :: Transform2D)
        <> bundle (color 0.2 0.2 0.2 1)
        <> bundle (Parent windowEntity)

  -- Spawn paddles
  let spawnPaddle pos =
        spawn $
          bundle (Rectangle paddleW paddleH)
            <> bundle (transform2d {transformTranslation = pos} :: Transform2D)
            <> bundle (color 1 1 1 1)
            <> bundle (Parent windowEntity)
            <> bundle (Paddle 0)

  leftPaddleE <- spawnPaddle (V2 (round paddleMargin) (round centerY))
  rightPaddleE <- spawnPaddle (V2 (round $ fromIntegral windowW - paddleMargin) (round centerY))

  -- Spawn ball
  ballE <-
    spawn $
      bundle (Circle ballR 16)
        <> bundle (transform2d {transformTranslation = V2 (round centerX) (round centerY)} :: Transform2D)
        <> bundle (color 1 1 0 1)
        <> bundle (Parent windowEntity)
        <> bundle (Ball ballInitSpeedX ballInitSpeedY)

  -- Run the game loop
  runAccessGLFW $ run windowEntity leftPaddleE rightPaddleE ballE

run :: EntityID -> EntityID -> EntityID -> EntityID -> Access GameM Bool
run windowE leftE rightE ballE = do
  now <- liftIO getCurrentTime
  lastTime <- lift $ gets gsLastTime
  let dt = realToFrac (diffUTCTime now lastTime) :: Float
      dt' = min dt 0.1
  lift . modify' $ \gs -> gs {gsLastTime = now}

  mKeys <- lookup @_ @Keys windowE
  case mKeys of
    Just keys -> do
      if keyJustPressed Key'Escape keys
        then return True
        else do
          updatePaddle leftE keys Key'W Key'S dt'
          updatePaddle rightE keys Key'Up Key'Down dt'
          updateBall dt' ballE leftE rightE
          render
          return False
    Nothing -> do
      render
      return False

updatePaddle :: EntityID -> Keys -> Key -> Key -> Float -> Access GameM ()
updatePaddle paddleE keys upKey downKey dt = do
  mTransform <- lookup @_ @Transform2D paddleE
  case mTransform of
    Just t -> do
      let V2 x y = transformTranslation t
          dy
            | keyPressed upKey keys = paddleSpeed * dt
            | keyPressed downKey keys = -paddleSpeed * dt
            | otherwise = 0
          halfH = paddleH / 2
          newY = max halfH (min (fromIntegral windowH - halfH) (fromIntegral y + dy))
      when (dy /= 0)
        . insert paddleE
        $ bundle (t {transformTranslation = V2 x (round newY)} :: Transform2D)
    Nothing -> return ()

updateBall :: Float -> EntityID -> EntityID -> EntityID -> Access GameM ()
updateBall dt ballE leftE rightE = do
  mBallT <- lookup @_ @Transform2D ballE
  mBall <- lookup @_ @Ball ballE
  mLeftT <- lookup @_ @Transform2D leftE
  mRightT <- lookup @_ @Transform2D rightE

  case (mBallT, mBall, mLeftT, mRightT) of
    (Just ballT, Just ball, Just leftT, Just rightT) -> do
      -- Get current velocity from Ball component
      let velX = ballVelX ball
          velY = ballVelY ball

      let V2 bx by = transformTranslation ballT
          V2 _ ly = transformTranslation leftT
          V2 _ ry = transformTranslation rightT

          bxF = fromIntegral bx :: Float
          byF = fromIntegral by :: Float
          leftY = fromIntegral ly :: Float
          rightY = fromIntegral ry :: Float

          -- Move ball
          newBx = bxF + velX * dt
          newBy = byF + velY * dt

          -- Wall collisions
          (!clampedBy, !velY')
            | newBy - ballR <= 0 = (ballR, abs velY)
            | newBy + ballR >= fromIntegral windowH = (fromIntegral windowH - ballR, -(abs velY))
            | otherwise = (newBy, velY)

          -- Paddle collision detection
          paddleHH = paddleH / 2
          leftPaddleX = paddleMargin
          rightPaddleX = fromIntegral windowW - paddleMargin

          hitLeft =
            newBx - ballR <= leftPaddleX + paddleW / 2
              && bxF - ballR >= leftPaddleX - paddleW / 2
              && clampedBy >= leftY - paddleHH
              && clampedBy <= leftY + paddleHH

          hitRight =
            newBx + ballR >= rightPaddleX - paddleW / 2
              && bxF + ballR <= rightPaddleX + paddleW / 2
              && clampedBy >= rightY - paddleHH
              && clampedBy <= rightY + paddleHH

          centerX = fromIntegral windowW / 2
          centerY = fromIntegral windowH / 2

      -- Determine potential winner, final position and velocity
      let (res, !finalBx, !finalBy, !velX') =
            if hitLeft
              then (Nothing, leftPaddleX + paddleW / 2 + ballR, clampedBy, abs velX)
              else
                if hitRight
                  then (Nothing, rightPaddleX - paddleW / 2 - ballR, clampedBy, -(abs velX))
                  else
                    if newBx + ballR < 0
                      then do
                        (Just (rightE, "Right"), centerX, centerY, ballInitSpeedX)
                      else
                        if newBx - ballR > fromIntegral windowW
                          then (Just (leftE, "Left"), centerX, centerY, -ballInitSpeedX)
                          else (Nothing, newBx, clampedBy, velX)

      -- Scoring
      case res of
        Just (e, s) -> do
          mPaddle <- lookup @_ @Paddle e
          let !score = case mPaddle of
                Just p -> paddleScore p + 1
                Nothing -> 1
          insert e . bundle $ Paddle score
          liftIO $ do
            putStrLn $ s ++ " scores! Score: " ++ show score
            hFlush stdout
        Nothing -> return ()

      -- Update ball transform and velocity
      insert ballE $
        bundle (ballT {transformTranslation = V2 (round finalBx) (round finalBy)} :: Transform2D)
          <> bundle (Ball velX' velY')
    _ -> return ()
