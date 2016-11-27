module Chapter17_applicative where

import Control.Applicative

import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Function

foo1:: IO String
foo1 = (++) <$> getLine <*> getLine

foo2:: IO String
foo2 = liftA (++) getLine <*> getLine

foo3:: IO Int
foo3 = fmap length $ (++) <$> getLine <*> getLine

foo4:: IO Int
foo4 = liftA2 (\x y -> length (x ++ y)) getLine getLine
