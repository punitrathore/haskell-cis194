-- Working my way through exercises here -> https://wiki.haskell.org/Typeclassopedia#Functor
{-# LANGUAGE FlexibleContexts #-}

module Functor where

data Option e a = Err e | Ok a

instance Functor (Option e) where
  fmap _ (Err e) = Err e
  fmap f (Ok a) = Ok (f a)

-- instance Functor ((->) e) where
--   fmap f g = g . f

-- fmap :: Functor f => (a -> b) -> f a -> f b

data Pair a = Pair a a deriving (Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

data Tuple a b = Tuple a b deriving (Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor ITree where
  fmap g (Leaf f) = Leaf (g . f)
  fmap f (Node its) = Node (map (fmap f) its)

a = Node [(Leaf (+1)), (Node [(Leaf (*3))])]
b = Leaf (+1)

foo seed init fn (Leaf f) = f seed
foo seed init fn (Node its) = foldr (\e acc -> acc `fn` (foo seed init fn e)) init its



f = Pair . Just
g = Tuple . Just
p = f 10
t = g 20
