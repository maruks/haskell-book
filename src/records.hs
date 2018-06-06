{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns, RecordWildCards  #-}

module Records (S(foo), T(..)) where

data S = MkS { foo :: Int } deriving (Show)
data T = MkT { foo :: Int, bar :: String } deriving (Show)
data U = MkU { bar :: Int, baz :: Int } deriving (Show)

f1 MkT {foo = f} = f

f2 x = baz x

f3 x = x { baz = 8 }

f4 x = x {foo = 0, bar = "OK" }

f5 :: T -> Int
f5 = foo       -- bad x = foo x

f6 s = foo (s :: S)

-- field puns

f8 MkT{foo} = foo

t1 = let foo = 2
         bar = "ok" in MkT{..}

f9 bar baz = MkU{..}
