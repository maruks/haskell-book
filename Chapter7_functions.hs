module Chapter7_functions where

main :: IO()
main = undefined

bindExp :: Integer -> String
bindExp x = let c = 5
                y = 10 in
             "x = " ++ show x ++ " y = " ++ show y

addOneIfOdd :: Int -> Int
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1

addFive :: Int -> Int -> Int
addFive = \x y -> (if x > y then y else x) + 5

f2 :: (a,b,c) -> (d,e,f) -> ((a,d),(c,f))
f2 (a,_,c) (d,_,f) = ((a,d),(c,f))
