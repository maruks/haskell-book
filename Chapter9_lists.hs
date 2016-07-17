module Chapter9_lists where

import Data.Bool (bool)

import Data.Char

myWords:: [Char] -> [[Char]]
myWords [] = []
myWords xs = takeWhile (/=' ') xs : myWords( dropWhile (==' ') . dropWhile (/=' ') $ xs)

sentences :: String
sentences  = "Tyger Tyger, burning bright\n" ++ "In the forests of the night\n" ++  "What immortal hand or eye\n" ++ "Could frame thy fearful symmetry?"

myLines :: Char -> String -> [String]
myLines _ [] = []
myLines c xs = takeWhile (/= c) xs : myLines c x
  where x = dropWhile (== c) . dropWhile (/= c) $ xs

mapBool :: [Int] -> [Int]
mapBool = map (\x -> bool (x) (-x) (x==3))

myFilter :: String -> [String]
myFilter = f . myWords
  where f = filter (\x -> x /= "the" && x /= "a" && x /= "an")

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip :: [a] -> [b] -> [(a,b)]
myZip = myZipWith (\a b -> (a,b))

capit :: String -> String
capit (x:xs) = toUpper x : xs
capit [] = []

allCaps :: String -> String
allCaps (x:xs) = toUpper x : allCaps xs
allCaps [] = []

capFirst :: String -> Char
capFirst = toUpper . head

caesar :: Char -> Char
caesar c = chr ( 97 + ((ord c) - 92) `mod` 26 )

unCaesar :: Char -> Char
unCaesar c = chr ( 97 + ( (ord c) - 102) `mod` 26 )

cipherText :: String -> String
cipherText = map caesar
