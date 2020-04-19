module Display where

import GameState

import HGE2D.Colors
import HGE2D.Types

data FieldType =
  FWall |
  FEmpty |
  FSnakeHead |
  FSnakeBody |
  FFood

makeDisplayMap :: GameState -> [[FieldType]]
makeDisplayMap gs = [[getFieldType x y | y <- [-1 .. boardSize]] | x <- [-1 .. boardSize]]
  where
    getFieldType x y
      | (x, y) == head (snake gs) = FSnakeHead
      | (x, y) `elem` (snake gs) = FSnakeBody
      | Just (x, y) == (food gs) = FFood
      | x == -1 = FWall
      | y == -1 = FWall
      | x == boardSize = FWall
      | y == boardSize = FWall
      | otherwise = FEmpty

makeDisplayField :: FieldType -> GlColorRGB
makeDisplayField FWall = colorRock
makeDisplayField FEmpty = colorGrass
makeDisplayField FSnakeHead = colorWhite
makeDisplayField FSnakeBody = colorBlack
makeDisplayField FFood = colorWater
