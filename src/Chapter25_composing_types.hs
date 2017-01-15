{-# LANGUAGE InstanceSigs #-}
module Chapter25_composing_types where

import Control.Monad.Identity
import Control.Monad.Trans.Identity

import Control.Applicative
import Control.Monad

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- fmap (+1) (Compose [Just 1, Nothing])

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

-- pure (+1) <*> (Compose [Just 1])

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap = undefined

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse = undefined

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

--1.
data Deux a b = Deux a b deriving(Eq, Show)
--2.
data Const a b = Const a deriving(Eq, Show)
--3.
data Drei a b c = Drei a b c deriving(Eq, Show)
--4.
data SuperDrei a b c = SuperDrei a b deriving(Eq, Show)
--5.
data SemiDrei a b c = SemiDrei a deriving(Eq, Show)
--6.
data Quadriceps a b c d = Quadzzz a b c d deriving(Eq, Show)
--7.
data Either a b = Left a | Right b deriving(Eq, Show)
