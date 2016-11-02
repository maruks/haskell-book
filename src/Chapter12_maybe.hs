module Chapter12_maybe where

import Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe' :: [String] -> [String]
replaceThe' [] = []
replaceThe' (s:xs) = repl : replaceThe' xs where
  repl = case notThe s of
           Nothing -> "a"
           Just _ -> s

replaceThe :: String -> String
replaceThe = unwords . replaceThe' . words


-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1

countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' (s1:xs@(s2:_)) = i + countTheBeforeVowel' xs where
  i = case (notThe s1, head s2 `elem` "aeiou") of
        (Nothing, True) -> 1
        _ -> 0
countTheBeforeVowel' _ = 0

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countTheBeforeVowel' . words


countVowels :: String -> Int
countVowels = length . filter (`elem` "aeiou")


newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if v > length s - v then Nothing else Just (Word' s) where
  v = countVowels s

--

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat' :: Integer -> Nat
integerToNat' 0 = Zero
integerToNat' n = Succ (integerToNat' (n - 1) )

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n >= 0 = Just (integerToNat' n)
  | otherwise = Nothing

-- maybe

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just v) = f v
mayybee d _ _ = d

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just v) = v
fromMaybe d _ = d

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe _ = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just v) = [v]
maybeToList _ = []

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if length xs == length ms then Just ms else Nothing where
  ms = catMaybes xs

-- either

lefts' :: [Either a b] -> [a]
lefts' = foldr na [] where
  na e a = case e of
         (Left l) -> l : a
         _ -> a

rights' :: [Either a b] -> [b]
rights' = foldr na [] where
  na e a = case e of
         (Right rl) -> rl : a
         _ -> a

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs )

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left l) = Nothing
eitherMaybe' f (Right r) = Just $ f r

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 _ (Left l) = f1 l
either' _ f2 (Right r) = f2 r

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- unfold


myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a )

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Nothing -> []
                  Just (a1, b1) -> a1 : myUnfoldr f b1

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\e -> Just (e, f e))

-- binary tree
