module Chapter6_typeclasses where
import Data.List (sort)

main :: IO()
main = undefined

-- Eq

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two c d) = a == b && c == d

data StringOrInt =
  TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (Pair a b) == (Pair c d) = a==c && b==d

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple c d = a==c && b==d

data Which a =
  ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False

data EitherOr a b =
  Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a==b
  (==) (Goodbye a) (Goodbye b) = a==b
  (==) _ _ = False

-- type fixes

data Person = Person Bool deriving (Show, Eq, Ord)
printPerson :: Person -> IO()
printPerson p = putStrLn $ show p

data Mood = Blah | Woot deriving (Eq, Show, Ord)
settleDown :: Mood -> Mood
settleDown x = if x < Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "foo" "bar" "lol"

-- typechecks

data Rocks = Rocks String deriving (Eq, Show, Ord)
data Yeah = Yeah Bool deriving (Eq, Show, Ord)
data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

papu :: Papu
papu =  Papu ( Rocks "chases" ) ( Yeah True )

eqForAll :: Papu -> Papu -> Bool
eqForAll p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p1 p2 = p1 > p2

-- types

i :: Num a => a
i = 1

--f :: Num a => a    -- error
--f :: Real a => a  --error
--f :: Float         -- ok
--f :: Fractional a => a  -- ok
f :: RealFrac a => a
f = 1.0

--freud :: Ord a => a -> a  -- ok
freud :: a -> a   -- identity
freud x = x

freud' :: Int -> Int
freud' x = x

myX :: Int
myX = 1

--sigmund :: Num a => a -> a --error
--sigmund :: a -> a --error
sigmund :: Int -> Int
sigmund _ = myX

--jung :: Ord a => [a] -> a
jung :: String -> Char
jung xs = head (sort xs)

--sort :: Ord a => [a] -> [a]

mySort :: [Char] -> [Char]
mySort = sort

--signifier :: Ord a => [a] -> a   --error
signifier :: [Char] -> Char
signifier xs = head $ mySort xs

-- types

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn a b = b == fn a

arith :: Num b => (a -> b) -> Integer -> a -> b
arith fn _ a = (fn a)
