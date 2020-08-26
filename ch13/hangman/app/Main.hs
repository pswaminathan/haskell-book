module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           GameState
import           Puzzle
import           System.Exit   (exitSuccess)
import           System.IO     (BufferMode (NoBuffering), hSetBuffering, stdout)
import           Words


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  putStrLn ""
  checkGameState puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character!"


checkGameState :: Puzzle -> IO ()
checkGameState puzzle = case gameState puzzle of
                          Lost       -> handleLoss puzzle
                          Won        -> handleWin puzzle
                          InProgress -> return ()


handleLoss :: Puzzle -> IO ()
handleLoss (Puzzle word _ _) = do
  putStrLn "You lose!"
  putStrLn $ "The word was: " ++ word
  exitSuccess

handleWin :: Puzzle -> IO ()
handleWin (Puzzle word _ _) = do
  putStrLn $ "You win! The word was: " ++ word
  exitSuccess


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = newPuzzle (fmap toLower word)
  runGame puzzle
