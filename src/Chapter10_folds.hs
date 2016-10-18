module Chapter10_folds where

import Data.Time

concFirst3 :: [[Char]] -> [Char]
concFirst3 = foldr ((++) . take 3 ) ""

concFirst3Left :: [[Char]] -> [Char]
concFirst3Left = foldl ((++) . take 3) ""

data DatabaseItem = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]

theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
                , DbNumber 9001
                , DbString "Hello World!"
                , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)) ]

type Database = [DatabaseItem]

filterDbDate :: Database -> [UTCTime]
filterDbDate = foldr filtTime [] where
  filtTime (DbDate t) a = t : a
  filtTime _ a = a

filterDbNumber :: Database -> [Integer]
filterDbNumber = foldr filtNum [] where
  filtNum (DbNumber t) a = t : a
  filtNum _ a = a

mostRecent :: Database -> UTCTime
mostRecent = foldr filtTime (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)) where
  filtTime (DbDate t) a = max t a
  filtTime _ a = a

sumDb :: Database -> Integer
sumDb = foldr filtNum 0 where
  filtNum (DbNumber t) a = t + a
  filtNum _ a = a

avgDb :: Database -> Double
avgDb db = fromIntegral total / fromIntegral len where
  total = sumDb db
  len = length $ filterDbNumber db

fibn :: Int -> Integer
fibn n = fibs !! n where
  fibs = 1 : scanl (+) 1 fibs

fib100 :: [Integer]
fib100 = takeWhile (<100) fibs where
  fibs = 1 : scanl (+) 1 fibs

factorial :: Int -> Integer
factorial n = fact !! n where
  fact = scanl (*) 1 [1..]


foo :: [(Char, Char, Char)]
foo = [ (a,b,c) | a <- stops, b <- vowels, c <- stops, a=='p'] where
  stops = "pbtdkg"
  vowels = "aeiou"

seekritFunc :: String -> Double
seekritFunc txt = fromIntegral x / fromIntegral y where
  x = sum $ map length $ words txt
  y = length $ words txt

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\e a -> a || f e) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (\e -> e == a)

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 _ [] = False
myElem1 a (x:xs)
  | a==x = True
  | otherwise = myElem1 a xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = foldr (\e a -> a || e == x) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\e a -> f e : a) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\e a -> if (f e) then e : a else a) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap:: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\e a -> f e ++ a) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\e a -> if f e a == GT then e else a) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\e a -> if f e a == LT then e else a) (head xs) xs
