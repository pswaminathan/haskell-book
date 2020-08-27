module Palindrome where

import           Control.Monad
import           Data.Char     (isAlpha, toLower)
import           System.Exit   (exitSuccess)


palindrome :: IO ()
palindrome = forever $ do
  putStrLn ""
  putStrLn "Is it a palindrome?"
  putStr "> "
  line1 <- getLine
  let normalized = normalize line1
   in case (normalized == reverse normalized) of
        True  -> putStrLn "It's a palindrome!"
        False -> do
          putStrLn "Nope!"
          exitSuccess


normalize :: String -> String
normalize = map toLower . filter isAlpha
