{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Chapter11_adt where

import Data.Char (toUpper, isLetter, isUpper, toLower)
import Data.Map.Strict (Map)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Monoid

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

-- phone exercise

type Buttons = [(Char, String)]

phoneButtons :: Buttons
phoneButtons =
  [ ('1', "")
  , ('2', "abc")
  , ('3', "def")
  , ('4', "ghi")
  , ('5', "jkl")
  , ('6', "mno")
  , ('7', "pqrs")
  , ('8', "tuv")
  , ('9', "wxyz")
  , ('0', " +_")
  , ('#', ".,")]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

createMappings :: (Char, String) -> [(Char, (Digit, Presses))]
createMappings (digit, xs) = [ (ch, (digit, num))  | (num, ch) <- t]
  where
    t = zip [1..] $ xs++[digit]

createAllMappings:: Buttons -> (Map Char (Digit, Presses))
createAllMappings bs = Map.fromList $ foldMap createMappings bs

data DaPhone = DaPhone (Map Char (Digit, Presses)) deriving (Show)

myPhone :: DaPhone
myPhone = DaPhone $ createAllMappings phoneButtons

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]

reverseTaps (DaPhone phone) c = if isUpper c then ('*',1) : [ps] else [ps]
  where
    ps = phone Map.! toLower c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]

cellPhonesDead p = foldMap (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses

fingerTaps xs = getSum $ foldMap (Sum . snd) xs

freqs
  :: (Eq a)
  => [a] -> [(Int, a)]

freqs xs = [ (n, e) | e <- es, let n = length $ filter (== e) xs]
  where es = nub xs

mostExpensiveWord :: DaPhone -> String -> (Int, String)

mostExpensiveWord phone str = last $ sortOn fst wExps
  where
    ws = words str
    wExps = [ (e, w) | w <- ws, let e = fingerTaps $ cellPhonesDead phone w]

coolestLtr :: [String] -> (Int, Char)

coolestLtr = last . sortOn fst . freqs . concat

coolestWord :: [String] -> (Int, String)

coolestWord = last . sortOn fst . freqs . concatMap words

-- Hutton’s Razor

data Expr = Lit Integer
  | Add Expr Expr deriving (Show, Eq)

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

instance Num Expr where
  (+) :: Expr -> Expr -> Expr
  (+) = Add
  fromInteger = Lit
