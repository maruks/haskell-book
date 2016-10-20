module Chapter11_adt where

import Data.Char (toUpper)

-- binary tree

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

insert'
  :: Ord a
  => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

fromList
  :: Ord a
  => [a] -> BinaryTree a
fromList = foldr insert' Leaf

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ (a : inorder right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

--

isSubsequenceOf
  :: (Eq a)
  => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf a@(x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf a ys
isSubsequenceOf _ [] = False

upWord :: String -> String
upWord (x:xs) = toUpper x : xs

words' :: String -> String -> [String] -> [String]
words' [] wAcc rAcc = reverse $ reverse wAcc : rAcc
words' (x:xs) wAcc rAcc
  | x == ' ' = words' xs [] $ reverse wAcc : rAcc
  | otherwise = words' xs (x:wAcc) rAcc

capitalizeWords :: String -> [(String,String)]
capitalizeWords s = zipWith (\x y -> (x,y)) words (map upWord words)
  where words = words' s [] []

capWord :: String -> String
capWord (x:xs)
  | x==' ' = x : capWord xs
  | otherwise = toUpper x : xs

capitalizeParagraph' :: String -> String -> String
capitalizeParagraph' [] wAcc = wAcc
capitalizeParagraph' (x:xs) wAcc
  | x == '.' = capWord (reverse (x:wAcc)) ++ capitalizeParagraph' xs []
  | otherwise = capitalizeParagraph' xs (x:wAcc)

capitalizeParagraph :: String -> String
capitalizeParagraph s = capitalizeParagraph' s []
