{-# LANGUAGE InstanceSigs #-}
module Chapter23_state where

import Control.Monad
import Control.Applicative

import Control.Monad.Trans.State
import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

-- state :: Monad m => (s -> (a, s)) -> StateT s m a

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

-- replicateM :: Monad m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s

-- evalState (nDie 3) (mkStdGen 3)

-- randomIO :: Random a => IO a

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n = go 0 0 []
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count dies gen
      | sum >= n = (count, dies)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) (intToDie die : dies) nextGen

-- state
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi
      (\s ->
         let (a', s') = g s
         in (f a', s'))

-- runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0 = (1,0)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi
      (\s ->
         let (a2b, fs) = f s
             (ga, gs) = g s
         in (a2b ga, gs))

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi
                  (\s ->
                     let (a1,s1) = f s
                         (Moi gs) = g a1
                        in gs s1)
--

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

--execState :: State s a -> s -> s

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = fizzbuzzList [to,(to-1)..from]

--type State s = StateT s Data.Functor.Identity.Identity :: * -> *
--StateT :: (s -> m (a, s)) -> StateT s m a

--get :: Monad m => StateT s m s
--put :: Monad m => s -> StateT s m ()

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzFromTo 1 100

--
getS :: Moi s s
getS = Moi (\s -> (s,s))

-- runMoi getS "curryIsAmaze"

putS :: s -> Moi s ()
putS v = Moi $ const ((),v)

exec :: Moi s a -> s -> s
exec (Moi sa) s = let (a1, s1) = sa s in s1

evalS :: Moi s a -> s -> a
evalS (Moi sa) a = let (a1,s1) = sa a in a1

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))
