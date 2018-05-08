{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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

-- Exericse 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: (Stream a) -> (Stream a) -> (Stream a)
interleaveStreams (Stream x1 sx) sy = Stream x1 (interleaveStreams sy sx)

startRuler :: Integer -> Stream Integer
startRuler y = interleaveStreams (streamRepeat y) (startRuler (y + 1))

ruler = startRuler 0

-- Exercise 6
x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))


instance Num (Stream Integer) where
  fromInteger n = Stream n (streamRepeat 0)
  negate = streamMap (0-)
  (+) (Stream s1 sx) (Stream s2 sy) = Stream (s1 + s2) (sx + sy)
  (*) (Stream x sx) ssy@(Stream y sy) = Stream (x*y) (sy*(fromInteger x) + sx*ssy)

instance Fractional (Stream Integer) where
  (/) (Stream x sx) (Stream y sy) = q
    where q = Stream (div x y) (streamMap (`div` y) (sx - q*sy))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7

data Matrix a = Matrix a a a a deriving(Show)

instance Num (Matrix Integer) where
  (*) (Matrix m1 m2 m3 m4) (Matrix n1 n2 n3 n4) = Matrix (m1*n1 + m2*n3) (m1*n2 + m2*n4) (m3*n1 + m4*n3) (m3*n2 + m4*n4)

m :: Matrix Integer
m = Matrix 1 1 1 0

fibs4' :: Integer -> (Matrix Integer)
fibs4' n = foldr (*) m $ take (fromIntegral (n - 1)) $ repeat m

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = case fibs4' n of
  Matrix n1 n2 n3 n4 -> n2
