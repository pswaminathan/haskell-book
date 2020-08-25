module PoemLines where


firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines lines
    | not $ elem '\n' lines = [lines]
    | otherwise = let h = takeWhile (/= '\n') lines
                      t = myLines $ tail $ dropWhile (/= '\n') lines
                  in h : t

shouldEqual = [ "Tyger Tyger, burning bright"
              , "In the forests of the night"
              , "What immortal hand or eye"
              , "Could frame thy fearful symmetry?"
              ]

main :: IO ()
main =
  print $ "Are they equal? "
          ++ show (myLines sentences == shouldEqual)
