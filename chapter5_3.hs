module Chapter5_3 where

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = head $ f i

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w . q $ a

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
munge f1 g1 x = fst . g1 . f1 $ x
