module Chapter16_functors where

import Test.QuickCheck hiding (Failure, Success)

import Test.QuickCheck.Function


replaceWithP = const 'p'

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



-- 1
newtype Identity a = Identity a
-- 2
data Pair a = Pair a a
-- 3
data Two a b = Two a b
-- 4
data Three a b c = Three a b c
-- 5
data Three' a b = Three' a b b
-- 6
data Four a b c d = Four a b c d
-- 7
data Four' a b = Four' a a a b
