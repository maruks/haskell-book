module Main where

main :: IO() ; main = undefined

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a,b) = f a b

f1 :: a -> a -> a-> a ; f1 = undefined
x :: Char; x = undefined
-- :t f1 x

g :: a -> b -> c -> b ; g = undefined
-- :t g 0 'c' "foo"

h :: (Num a, Num b) => a -> b -> b ; h = undefined
-- :t h 1.0 2

j :: (Num a, Num b) => a -> b -> b ; j = undefined
-- :t j 1 (5.5 :: Double)

jackal :: (Ord a, Eq b) => a -> b -> a ; jackal = undefined
-- :t jackal "foo" "bar"
-- :t jackal "foo"

kessel :: (Ord a, Num b) => a -> b -> a ; kessel = undefined
-- :t kessel 1 2
-- :t kessel 1 (2 :: Integer)
-- :t kessel (1 :: Integer) 2

functionH :: [a] -> Maybe a
functionH (a : _) = Just a
functionH [] = Nothing

functionC :: (Ord a) => a -> a -> Bool
functionC m n = m > n

functionS :: (a,b) -> b
functionS (_,b) = b
