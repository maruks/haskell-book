{-# LANGUAGE InstanceSigs #-}
module Chapter25_monad_transformers where

import Control.Applicative (liftA2)
import Control.Monad

-- IdentityT
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) ::  IdentityT m a -> (a ->  IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f     -- runIdentityT . f ::   a -> m b

-- (fmap . fmap) :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)

-- MaybeT
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

instance (Functor m) =>
         Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) =>
         Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ liftA2 (<*>) fab mma -- <$> fab <*> mma

instance (Monad m) =>
         Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f =       -- ma :: m (Maybe a)
    MaybeT $ do
      v <- ma                -- v :: Maybe a
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)  -- runMaybeT (f y) :: m b

-- EitherT
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT m) = EitherT $ (fmap . fmap) f m

instance Applicative m => Applicative (EitherT e m) where
  pure = pure
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT fab) <*> (EitherT ema) = EitherT $ liftA2 (<*>) fab ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT me) >>= f =
    EitherT $ do
      e <- me
      case e of
        Left a -> return (Left a)
        Right b -> runEitherT (f b)

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT f) = EitherT $ swapEither <$> f

-- catamorphism
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT m) = do
  e <- m
  case e of
    Left l -> fa l
    Right r -> fb r

-- ReaderT
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT s m) where
  fmap f (ReaderT r) = ReaderT $ (fmap . fmap) f r -- fmap f . r

instance (Monad m) => Applicative (ReaderT s m) where
  pure = undefined
  (<*>) = undefined

instance (Monad m) => Monad (ReaderT s m) where
  return = pure
  sma >>= f = undefined

-- StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f m = undefined

instance (Monad m) => Applicative (StateT s m) where
  pure = undefined
  (<*>) = undefined

instance (Monad m) => Monad (StateT s m) where
  return = pure
  sma >>= f = undefined
