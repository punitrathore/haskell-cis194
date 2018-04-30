module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = map snd $
        iterate (\(x,y) -> (y, x + y)) (0,1)

-- Exercise 3

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a:(streamToList s)

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a s) = (Stream (f a) (streamMap f s))

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f init = let nextVal = f init in
                          (Stream init (streamFromSeed f nextVal))
