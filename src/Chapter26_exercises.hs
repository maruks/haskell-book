module Chapter26_exercises where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Functor.Identity

import Control.Monad.Trans.Maybe
import Control.Monad

import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

-- 26.14 Chapter Exercises

rDec :: Num a => Reader a a
rDec = ReaderT $ return . subtract 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT (\a -> do
                           print ("Hi " ++ show a)
                           return (a + 1))

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT (\a -> do
                            print ("Hi " ++ show a)
                            return (show a, 1 + a))

------

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
