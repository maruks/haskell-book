module OuterInner where

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- readerUnwrap ()
-- Right (Just 1)

embedded2 :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded2 = let i:: ReaderT () IO (Either String (Maybe Int))
                i = ReaderT (const $ return (Right (Just 1)))
            in MaybeT $ ExceptT i

embedded3 :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded3 = MaybeT $ ExceptT $ ReaderT $ return . const (Right (Just 1))
