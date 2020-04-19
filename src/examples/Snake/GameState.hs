module GameState where

import System.Random

import HGE2D.Types

boardSize :: Int
boardSize = 40

stepLen :: Millisecond
stepLen = 500

data Direction = DUp | DDown | DLeft | DRight
  deriving Eq
data Pause = Pause | NoPause
  deriving Eq
data Lost = Lost | NotLost
  deriving Eq

type Place = (Int, Int)
type Snake = [Place]

data GameState = GameState
    { gsSize    :: (Double, Double) -- current size of the entire game in pixels
    , time      :: Millisecond      -- current time of the game
    , snake     :: Snake
    , snakeDirection :: Direction
    , pause     :: Pause
    , lost      :: Lost
    , food      :: Maybe Place
    , randGen   :: Maybe StdGen
    }

initialGameState :: GameState
initialGameState = GameState
    { gsSize = (0.0, 0.0)
    , time = 0
    , snake = [(13, 13)]
    , snakeDirection = DDown
    , pause = Pause
    , lost = NotLost
    , food = Nothing
    , randGen = Nothing
    }
