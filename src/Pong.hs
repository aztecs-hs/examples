{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs
import Aztecs.GL.D2
import Aztecs.GL.Text
import Aztecs.GLFW
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Time.Clock
import Prelude hiding (lookup)

-- Configuration options

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

-- Current game state
newtype GameState = GameState {lastTime :: UTCTime}

-- Game monad
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
  let initialState = GameState {lastTime = now}
  (_, _) <- runStateT (runAccess_ run) initialState
  putStrLn "Game ended!"

run :: Access GameM ()
run = do
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

  -- Spawn font and score labels
  font <- spawn . bundle $ Font "assets/Montserrat-VariableFont_wght.ttf"

  leftScoreLabelE <-
    spawn $
      bundle (Label "0" font defaultLabelStyle {labelFontSize = 48, labelColor = V4 1 1 1 1})
        <> bundle (transform2d {transformTranslation = V2 (round (centerX - 60)) (round (fromIntegral windowH - 50 :: Float))} :: Transform2D)
        <> bundle (Parent windowEntity)

  rightScoreLabelE <-
    spawn $
      bundle (Label "0" font defaultLabelStyle {labelFontSize = 48, labelColor = V4 1 1 1 1})
        <> bundle (transform2d {transformTranslation = V2 (round (centerX + 40)) (round (fromIntegral windowH - 50 :: Float))} :: Transform2D)
        <> bundle (Parent windowEntity)

  -- Run the game loop
  let rs =
        Loop
          { window = windowEntity,
            leftPaddle = leftPaddleE,
            rightPaddle = rightPaddleE,
            ball = ballE,
            leftScoreLabel = leftScoreLabelE,
            rightScoreLabel = rightScoreLabelE
          }
  runAccessGLFW $ loop rs

data Loop = Loop
  { window :: EntityID,
    leftPaddle :: EntityID,
    rightPaddle :: EntityID,
    ball :: EntityID,
    leftScoreLabel :: EntityID,
    rightScoreLabel :: EntityID
  }

loop :: Loop -> Access GameM Bool
loop rs = do
  now <- liftIO getCurrentTime
  lt <- lift $ gets lastTime
  let dt = realToFrac (diffUTCTime now lt) :: Float
      dt' = min dt 0.1
  lift . modify' $ \gs -> gs {lastTime = now}

  mKeys <- lookup @_ @Keys (window rs)
  case mKeys of
    Just keys -> do
      if keyJustPressed Key'Escape keys
        then return True
        else do
          updatePaddle (leftPaddle rs) keys Key'W Key'S dt'
          updatePaddle (rightPaddle rs) keys Key'Up Key'Down dt'
          updateBall dt' rs
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

updateBall :: Float -> Loop -> Access GameM ()
updateBall dt rs = do
  let ballE = ball rs
      leftE = leftPaddle rs
      rightE = rightPaddle rs
  mBallT <- lookup @_ @Transform2D ballE
  mBall <- lookup @_ @Ball ballE
  mLeftT <- lookup @_ @Transform2D leftE
  mRightT <- lookup @_ @Transform2D rightE

  case (mBallT, mBall, mLeftT, mRightT) of
    (Just ballT, Just ball', Just leftT, Just rightT) -> do
      -- Get current velocity from Ball component
      let velX = ballVelX ball'
          velY = ballVelY ball'

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
                      then (Just (rightE, False), centerX, centerY, ballInitSpeedX)
                      else
                        if newBx - ballR > fromIntegral windowW
                          then (Just (leftE, True), centerX, centerY, -ballInitSpeedX)
                          else (Nothing, newBx, clampedBy, velX)

      -- Scoring
      case res of
        Just (e, isLeft) -> do
          mPaddle <- lookup @_ @Paddle e
          let !score = case mPaddle of
                Just p -> paddleScore p + 1
                Nothing -> 1
          insert e . bundle $ Paddle score
          -- Update the appropriate score label
          let labelE = if isLeft then leftScoreLabel rs else rightScoreLabel rs
          mLabel <- lookup @_ @Label labelE
          case mLabel of
            Just lbl -> insert labelE . bundle $ lbl {labelText = show score}
            Nothing -> return ()
        Nothing -> return ()

      -- Update ball transform and velocity
      insert ballE $
        bundle (ballT {transformTranslation = V2 (round finalBx) (round finalBy)} :: Transform2D)
          <> bundle (Ball velX' velY')
    _ -> return ()
