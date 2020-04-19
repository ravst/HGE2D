module Time where

import Dynamics
import GameState

import HGE2D.Types

tickTime :: Millisecond
tickTime = 80

applyTime :: GameState -> Millisecond -> GameState
applyTime gs ms
    | pause gs == Pause = gs
    | lost gs == Lost = gs
    | otherwise = myIterate oneTick (ticsNo (time gs) ms) gs

ticsNo :: Millisecond -> Millisecond -> Int
ticsNo last passed =
    ((last `mod` tickTime) + passed) `div` tickTime

myIterate :: (a -> a) -> Int -> a -> a
myIterate _ 0 a = a
myIterate f n a = myIterate f (n - 1) (f a)
