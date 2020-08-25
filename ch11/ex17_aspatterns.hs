module AsPatterns where

import           Data.Char

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t


doubleUp :: [a] -> [a]
doubleUp []       = []
doubleUp xs@(x:_) = x : xs


isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _                       = True
isSubseqOf _ []                       = False
isSubseqOf needle@(n:ns) (h:haystack) = if n == h
                                           then isSubseqOf ns haystack
                                           else isSubseqOf needle haystack



capitalizeWords' :: String
                 -> [(String, String)]
capitalizeWords' = go . words
    where go []             = []
          go (w@(c:cs):wds) = (w, toUpper c : cs) : go wds



-- sure, as-patterns are nice. But the above is a little cryptic.
-- This feels more natural
capitalizeWords :: String
                -> [(String, String)]
capitalizeWords "" = []
capitalizeWords wds = [(wd, capitalizeWord wd)
                      | wd <- words wds]

capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (s:ss) = toUpper s : ss
