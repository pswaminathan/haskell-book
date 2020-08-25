module Cipher where

import           Data.Char

-- ch9

mkCaesar :: Int -> (String -> String)
mkCaesar n = map ( chr
                 . (+97)
                 . flip mod 26
                 . (+n)
                 . subtract 97
                 . ord )

mkUnCaesar :: Int -> (String, String)
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
