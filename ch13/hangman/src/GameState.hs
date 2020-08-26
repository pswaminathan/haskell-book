module GameState where

import           Data.Maybe    (isJust)
import           Puzzle

data GameState = InProgress
    | Won
    | Lost

gameState :: Puzzle -> GameState
gameState (Puzzle _ filledIn guessed)
    | all isJust filledIn = Won
    | length guessed > 6 = Lost
    | otherwise = InProgress
