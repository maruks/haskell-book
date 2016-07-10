module Chapter7_functions where

bindExp :: Integer -> String
bindExp x = let y = 10 :: Integer in
             "x = " ++ show x ++ " y = " ++ show y

addOneIfOdd :: Int -> Int
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1

addFive :: Int -> Int -> Int
addFive = \x y -> (if x > y then y else x) + 5

addFiveG :: Int -> Int -> Int
addFiveG x y
  | x > y = y + 5
  | otherwise = x + 5

f2 :: (a,b,c) -> (d,e,f) -> ((a,d),(c,f))
f2 (a,_,c) (d,_,f) = ((a,d),(c,f))

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where xLast = x `div` 100
        (_,d) = xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y z
  | z == True = x
  | otherwise = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z = case z of
  True -> x
  False -> y

g3 :: (a -> b) -> (a,c) -> (b,c)
g3 f (a,c) = ( f(a) , c )


roundTrip :: (Show a, Read b) => a -> b
roundTrip  = read . show

main :: IO()
main = do
  print ((roundTrip (4::Int) :: Int) )
  print (id (4::Int))

data SumOfThree a b c =
  First a
  | Second b
  | Third c
