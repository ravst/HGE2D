module Dynamics where

import Data.Maybe
import System.Random

import GameState

setRandomGenIfEmpty :: GameState -> GameState
setRandomGenIfEmpty gs = case randGen gs of
    Just _ -> gs
    Nothing -> gs {randGen = Just $ mkStdGen (time gs)}

oneTick :: GameState -> GameState
oneTick maybeNoRandomGs = gs {
      lost = isOk newSnake,
      snake = newSnake,
      food = newFood,
      randGen = Just newRandGen
  }
  where
    gs = setRandomGenIfEmpty maybeNoRandomGs
    oldSnake = snake gs
    oldSnakeHead = head oldSnake
    newSnakeHead = move oldSnakeHead (snakeDirection gs)
    newSnake' = newSnakeHead : oldSnake
    newSnake =
        if justEaten gs then newSnake' else init newSnake'
    Just oldRandGen = randGen gs -- there has to be a working generator at this point
    (newFood, newRandGen) = case food gs of
        Just food -> if food == newSnakeHead
                         then (Nothing, oldRandGen)
                         else (Just food, oldRandGen)
        Nothing -> mapFst Just $ genNewFood oldRandGen newSnake

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

isOk :: Snake -> Lost
isOk snake = if selfCross || outOfBound then Lost else NotLost
  where
    selfCross = head snake `elem` tail snake
    (hx, hy) = head snake
    outOfBound = or $ [
        hx < 0,
        hy < 0,
        hx >= boardSize,
        hy >= boardSize]

justEaten :: GameState -> Bool
justEaten = isNothing . food

move :: Place -> Direction -> Place
move (x, y) DUp = (x, y - 1)
move (x, y) DDown = (x, y + 1)
move (x, y) DLeft = (x - 1, y)
move (x, y) DRight = (x + 1, y)

genNewFood :: StdGen -> Snake -> (Place, StdGen)
genNewFood gen snake =
      let (x', gen') = next gen in
      let (y', gen'') = next gen' in
      let (x, y) = (x' `mod` boardSize, y' `mod` boardSize) in
      if (x,y) `elem` snake
      then genNewFood gen'' snake
      else ((x, y), gen'')

setDirection :: GameState -> Direction -> GameState
setDirection gs dir = if opposite dir (snakeDirection gs)
                         then gs
                         else gs {snakeDirection = dir}

opposite :: Direction -> Direction -> Bool
opposite DUp DDown = True
opposite DDown DUp = True
opposite DLeft DRight = True
opposite DRight DLeft = True
opposite _ _ = False
