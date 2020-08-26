module Puzzle where

import           Data.List (intersperse)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    let rep = intersperse ' ' $ fmap renderChar discovered
        renderChar :: Maybe Char -> Char
        renderChar Nothing  = '_'
        renderChar (Just c) = c
     in rep ++ " Guessed so far: " ++ guessed



newPuzzle :: String -> Puzzle
newPuzzle word = Puzzle word guesses []
  where guesses = [ Nothing | _ <- word ]

newPuzzle' :: String -> Puzzle
newPuzzle' word = Puzzle word guesses []
  where guesses = map (const Nothing) word

checkGuess :: Puzzle -> Char -> Bool
checkGuess (Puzzle string _ _) c = c `elem` string

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter puzzle@(Puzzle word filledIn guesses) c =
  Puzzle word newFilledIn newGuesses
    where newGuesses = if checkGuess puzzle c
                         then guesses
                         else c : guesses
          zipper guessed wordChar guessChar = if wordChar == guessed
                                                 then Just wordChar
                                                 else guessChar
          newFilledIn = zipWith (zipper c) word filledIn


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (checkGuess puzzle guess
       , alreadyGuessed puzzle guess) of

       (_, True) -> do
         putStrLn "You already guessed that character. \
                  \ Pick something else!"
         return puzzle

       (True, _) -> do
         putStrLn "This character was in the word! Filling \
                  \ in the word accordingly."
         return (fillInCharacter puzzle guess)

       (False, _) -> do
         putStrLn "This character was not in the word. Try again!"
         return (fillInCharacter puzzle guess)
