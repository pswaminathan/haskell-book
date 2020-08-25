{-# LANGUAGE InstanceSigs #-}

module BinaryTree where

data BinaryTree a = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

instance Functor BinaryTree where
  fmap :: (a -> b)
       -> BinaryTree a
       -> BinaryTree b
  fmap _ Leaf                = Leaf
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b
foldTree _ acc Leaf                  = acc
foldTree f acc (Node left val Leaf) = foldTree f (f val acc) left
foldTree f acc (Node Leaf val right) = foldTree f (f val acc) right
foldTree f acc (Node left val right) = foldTree f (foldTree f (f val acc) left) right


--

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 1 Leaf) 3 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 2 Leaf) 4 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOk :: IO ()
mapOk =
    if mapTree (+1) testTree' == mapExpected
       then print "yup ok!"
       else error "test failed!"

--

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)


inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

--

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
         2
         (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
       then putStrLn "Preorder fine!"
       else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
       then putStrLn "Inorder fine!"
       else putStrLn "Bad news bears."


testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
       then putStrLn "Postorder fine!"
       else putStrLn "Bad news bears."


main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
