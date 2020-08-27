module Person where

import           Control.Monad (when)
import           Data.Either   (fromLeft, fromRight, isLeft)
import           System.Exit   (exitSuccess)
import           Text.Read     (readEither)

type Name = String
type Age = Integer

data Person = Person Name Age
    deriving Show

data PersonInvalid = NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)


mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++
        " Age was: " ++ show age

handleAgeError :: IO ()
handleAgeError = do
  putStrLn "Not a valid number!"
  exitSuccess


gimmePerson :: IO ()
gimmePerson = do
  putStrLn ""
  putStr "Enter your name: "
  name <- getLine
  putStr "Enter your age: "
  ageStr <- getLine
  let eitherAge = readEither ageStr
      age = fromRight 1 eitherAge
      person = mkPerson name age
  when (isLeft eitherAge) handleAgeError

  if (isLeft person)
     then putStrLn ("An error occurred: " ++ (show $ fromLeft NameEmpty person))
     else putStrLn ("Yay! Successfully got a person: " ++ (show $ fromRight (Person "" 1) person))
