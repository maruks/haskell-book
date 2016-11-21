module Chapter16_functors where

import Test.QuickCheck hiding (Failure, Success)

import Test.QuickCheck.Function

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

-- 1
a = fmap (+1) $ read "[1]" :: [Int]


-- 2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3
c = fmap (*2) (\x -> x - 2)

-- 4
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) changed


-- functor laws

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1
newtype Identity a = Identity a deriving (Eq,Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 2
data Pair a = Pair a a deriving (Eq,Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

-- 3
data Two a b = Two a b deriving (Eq,Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4
data Three a b c = Three a b c deriving (Eq,Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type IntToInt = Fun Int Int
type IntThree = Three Int Int Int -> IntToInt -> IntToInt -> Bool

-- 5
data Three' a b = Three' a b b deriving (Eq,Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

type IntThree' = Three' Int Int -> IntToInt -> IntToInt -> Bool

-- 6
data Four a b c d = Four a b c d deriving (Eq,Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- 7
data Four' a b = Four' a a a b deriving (Eq,Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: Three Int Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three Int Int Int)
  quickCheck (functorCompose' :: IntThree)
  quickCheck $ \x -> functorIdentity (x :: Three' Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three' Int Int)
  quickCheck (functorCompose' :: IntThree')
