module Chapter5_types where

main :: IO()
main = do
  print $ 1 + 2
  putStrLn "10"
  print (negate 1)
  print ((+) 0 foo)
  where foo = negate 1

---

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a,b) = f a b

f1 :: a -> a -> a-> a ; f1 = undefined
x :: Char; x = undefined
-- :t f1 x

g :: a -> b -> c -> b ; g = undefined
-- :t g 0 'c' "foo"

h :: (Num a, Num b) => a -> b -> b ; h = undefined
-- :t h 1.0 2

j :: (Num a, Num b) => a -> b -> b ; j = undefined
-- :t j 1 (5.5 :: Double)

jackal :: (Ord a, Eq b) => a -> b -> a ; jackal = undefined
-- :t jackal "foo" "bar"
-- :t jackal "foo"

kessel :: (Ord a, Num b) => a -> b -> a ; kessel = undefined
-- :t kessel 1 2
-- :t kessel 1 (2 :: Integer)
-- :t kessel (1 :: Integer) 2

functionH :: [a] -> Maybe a
functionH (a : _) = Just a
functionH [] = Nothing

functionC :: (Ord a) => a -> a -> Bool
functionC m n = m > n

functionS :: (a,b) -> b
functionS (_,b) = b

---


f :: Int -> String
f = undefined

g' :: String -> Char
g' = undefined

h' :: Int -> Char
h' i = head $ f i

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x,y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f2 g1 = fst . g1 . f2
