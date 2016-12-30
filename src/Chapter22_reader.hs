{-# LANGUAGE InstanceSigs #-}
module Chapter18_reader where

import Data.Char
import Data.Maybe
import Data.Traversable

import Control.Monad
import Control.Applicative

-- Functor, Applicative, and Monad instances for functions

boopF :: Integer -> Integer
boopF = (+1) <$> (*2)

-- instance Applicative ((->) a)

boopA :: Integer -> Integer
boopA = (+) <$> (+1) <*> (*2)

-- (a -> a -> b) -> (a -> a) -> (a -> b)
-- (+) <$>  (+1)

boopL :: Integer -> Integer
boopL = liftA2 (+) (+1) (*2)

boopM :: Integer -> Integer
boopM = do
  a <- (+1)
  b <- (*2)
  return (a + b)

--
cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupledL :: String -> (String, String)
tupledL = liftA2 (,) rev cap

tupledA :: String -> (String, String)
tupledA = (,) <$> rev <*> cap

compM :: String -> String
compM = do
  a <- rev
  return (cap a)

compM2 :: String -> String
compM2 = rev >>= return cap

tupledM :: String -> (String, String)
tupledM = do
  a <- cap
  b <- cap
  return (a,b)

tupledM2 :: String -> (String, String)
tupledM2 =  rev >>= \x1 -> cap >>= \x2 -> return (x1, x2)

--
newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

--
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f aa ab = f <$> aa <*> ab

--
asks :: (r -> a) -> Reader r a
asks = Reader

-- applicative
instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (const a)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r ->
      let a2b = rab r
          a = ra r
      in a2b a

-- monad
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  --        (r -> a) -> (a -> (r -> b))  -> (r -> b)
  (Reader ra) >>= aRb =
    Reader $ \r ->
      let a = ra r
          rr2b = aRb a in
        runReader rr2b r

--
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

getDogA :: Person -> Dog
getDogA = Dog <$> dogName <*> address

getDogRM :: Reader Person Dog
getDogRM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy

chris :: Person
chris = Person
  (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")

-- runReader getDogRM chris

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

--
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

-- uncurry :: (a -> b -> c) -> (a, b) -> c
summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

--fromMaybe :: a -> Maybe a -> a
--sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
-- 1. fold the boolean conjunction operator over the list of results of sequA (applied to some value).
  print $ foldr (&&) False $ sequA 5
-- 2. apply sequA to s'; you’ll need fromMaybe.
  print $ sequA $ fromMaybe 5 s'
-- 3. apply bolt to ys; you’ll need fromMaybe.
  print $ bolt $ fromMaybe 5 ys
