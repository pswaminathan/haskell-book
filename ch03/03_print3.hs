module Print3 where

myGreeting :: String
myGreeting = "Hello" ++ " World!"

hello :: String
hello = "hello"

world :: String
world = "world"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
    where secondGreeting = concat [hello, " ", world]
  -- putStrLn "ending!"
  -- putStrLn let g = hello ++ " " ++ world
  --          in g
  -- let g = hello ++ " " ++ world
  --  in putStrLn g
