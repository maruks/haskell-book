module Chapter5_2 where

main :: IO()
main = do
  print $ 1 + 2
  putStrLn "10"
  print (negate 1)
  print ((+) 0 foo)
  where foo = negate 1
