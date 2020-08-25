module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
     then putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pssssh."
  where cool =
          coolness == "downright frosty yo"

fst' :: (a, b) -> a
fst' (a, b) = a

snd' :: (a, b) -> b
snd' (a, b) = b

tupFunc :: (Integer, [a])
        -> (Integer, [a])
        -> (Integer, [a])
tupFunc (a, b) (c, d) =
  ((a + c), (b ++ d))
