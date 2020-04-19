module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Colors
--import HGE2D.Classes
import HGE2D.Instances ()
import HGE2D.Shapes
import HGE2D.Render
import HGE2D.Engine

import GameState
import Time
import Display
import Dynamics

{- Example showing dynamic changes by moving the rectanlge -}
--------------------------------------------------------------------------------

--in here we are going to store all data of the game

--define all functions of the engine for usage of our state
stateUpdate = EngineState
    { getTitle = myGetTitle
    , getTime = myGetTime
    , setTime = mySetTime
    , moveTime = myMoveTime
    , click = myClick
    , mUp = myMouseUp
    , hover = myHover
    , drag = myDrag
    , kDown = myKeyDown
    , kUp = myKeyUp
    , resize = myResize
    , getSize = myGetSize
    , toGlInstr = myToGlInstr
    } :: EngineState GameState
  where
      myGetTitle _ = "HaskSnake" --title of the games window
      myGetTime = time -- how to retrieve the games time
      mySetTime ms gs = gs { time = ms } -- how to set the games time
      myMoveTime ms gs = applyTime gs ms
      myClick _ _ = id
      myMouseUp _ _ = id --nor mouse up
      myHover _ _ = id
      myDrag _ _ = id
      myKeyDown _ _ 'i' gs = setDirection gs DUp
      myKeyDown _ _ 'k' gs = setDirection gs DDown
      myKeyDown _ _ 'l' gs = setDirection gs DRight
      myKeyDown _ _ 'j' gs = setDirection gs DLeft
      myKeyDown _ _ 'r' gs = initialGameState {gsSize = (gsSize gs)}
      myKeyDown _ _ ' ' gs = case (pause gs) of
                               Pause -> gs {pause = NoPause}
                               NoPause -> gs {pause = Pause}
      myKeyDown _ _ _ gs = gs
      myKeyUp _ _ _ = id --nor key releases
      myResize (w, h) gs = gs { gsSize = (realToFrac w, realToFrac h) } -- how to resize our game
      myGetSize = gsSize -- and get its size
      myToGlInstr gs =
          withCamera stateUpdate gs $
          RenderPreserve $
          RenderMany $
          [
            displayBoard screenX screenY $ map (map makeDisplayField) $ makeDisplayMap gs
          ]
          where (screenY, screenX) = gsSize gs

makeSquare :: Float -> Float -> Float -> GlColorRGB -> RenderInstruction
makeSquare size x y color = RenderPreserve $ RenderMany [RenderColorize color, RenderTranslate x y, rectangle size size]

checkerBoard = [[if (i + j) `mod` 2 == 0 then colorRed else colorWhite | j <- [1..40]] |  i <- [1..40]]

displayBoard :: Double -> Double -> [[GlColorRGB]] -> RenderInstruction
displayBoard screenHeight screenWidth board = RenderMany $ concat $
    [[makeSquare' x y color | (y, color) <- zip [0..] row]
       | (x, row) <- zip [0..] board]
    where
      boardSize = length board -- assume the board is a square
      smallerDim = min screenHeight screenWidth
      margin = 60
      squareSize = realToFrac $ ((smallerDim - margin) / fromIntegral boardSize)
      boardOnScreenSize = squareSize * fromIntegral boardSize
      leftMargin = (screenWidth - boardOnScreenSize) / 2
      topMargin = (screenHeight - boardOnScreenSize) / 2
      makeSquare' x y colour = makeSquare
          (realToFrac squareSize)
          (realToFrac (x*squareSize + leftMargin))
          (realToFrac (y*squareSize + topMargin))
          colour


--------------------------------------------------------------------------------
main = runEngine stateUpdate initialGameState
