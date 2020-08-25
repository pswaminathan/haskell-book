module DoSyntax where


concatUserInput :: IO String
concatUserInput = do
  x1 <- getLine
  x2 <- getLine
  return (x1 ++ x2)


twoo :: IO Bool
twoo = do c  <- getChar
          c' <- getChar
          return (c == c')


main :: IO ()
main = do c  <- getChar
          c' <- getChar
          if c == c'
             then putStrLn "True"
             else return ()
