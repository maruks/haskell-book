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

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))
  (ReaderT r1) <*> (ReaderT r2) = ReaderT $ liftA2 (<*>) r1 r2
  --                              ReaderT $ (<*>) <$> r1 <*> r2

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  ReaderT rma >>= f = ReaderT (\r -> rma r >>= ($ r) . runReaderT . f)
  --                                        (\a -> runReaderT (f a) r)

-- ReaderT $ \r -> do
-- a <- rma r
-- runReaderT (f a)  r

-- StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) =>
         Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT st) = StateT (\s -> (\(a1, s1) -> (f a1, s)) <$> st s)

instance (Monad m) =>
         Applicative (StateT s m) where
  pure a = StateT (\s -> return (a, s))
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT mfab) <*> (StateT ma) =
    StateT
      (\s -> do
         (a2b, s1) <- mfab s
         (a2, s2) <- ma s1
         return (a2b a2, s2))

instance (Monad m) =>
         Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f =
    StateT
      (\s -> do
         (a1, s1) <- sma s
         runStateT (f a1) s)
