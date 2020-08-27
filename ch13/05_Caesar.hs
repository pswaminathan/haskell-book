module Caesar where

import           Control.Monad (forever)
import           Data.Char

main :: IO ()
main = forever $ do
  putStrLn ""
  putStr "Enter a word: "
  word <- getLine
  putStrLn $ "Your word encodes to: " ++ caesar word


mkCaesar :: Int -> (String -> String)
mkCaesar n = map ( chr
                 . (+97)
                 . flip mod 26
                 . (+n)
                 . subtract 97
                 . ord )


mkUnCaesar :: Int -> (String -> String)
mkUnCaesar n = map ( chr
                   . (+97)
                   . flip mod 26
                   . subtract n
                   . subtract 97
                   . ord )


caesar :: String -> String
caesar = mkCaesar 10

uncaesar :: String -> String
uncaesar = mkUnCaesar 10
