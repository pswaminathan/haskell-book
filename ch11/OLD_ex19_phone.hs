module Phone where

data Phone = Phone [PhoneTrieNode]
    deriving Show

data PhoneTrieNode = End Digit Char
    | Node Digit Char PhoneTrieNode
    deriving Show

phone = Phone [ End '0' ' '
              , Node '2' 'a' (Node '2' 'b' (Node '2' 'c' (End '2' '2')))
              , Node '3' 'd' (Node '3' 'e' (Node '3' 'f' (End '3' '3')))
              , Node '4' 'g' (Node '4' 'h' (Node '4' 'i' (End '4' '4')))
              , Node '5' 'j' (Node '5' 'j' (Node '5' 'j' (End '5' '5')))
              , Node '6' 'm' (Node '6' 'n' (Node '6' 'o' (End '6' '6')))
              , Node '7' 'p' (Node '7' 'q' (Node '7' 'r' (Node '7' 's' (End '7' '7'))))
              , Node '8' 't' (Node '8' 'u' (Node '8' 'v' (End '8' '8')))
              , Node '9' 'w' (Node '9' 'x' (Node '9' 'y' (Node '9' 'z' (End '9' '9'))))
              ]





tapsToChar :: [(Digit, Presses)] -> Char
tapsToChar [('*', 1), (digit, presses)] = go digits presses True
tapsToChar [(digit, presses)] = go digits presses False
    where go :: Digit -> Presses -> Bool -> PhoneTrieNode

mapIndexed :: ((Integer, a) -> b) -> [a] -> [b]
mapIndexed f = map f . zip [0..]

type Digit = Char

validButtons :: [Digit]
validButtons = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '*', '#']

-- Number of presses: >=1
type Presses = Int












fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd


