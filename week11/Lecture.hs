module Lecture where

(*>) :: Applicative f => f a -> f b -> f b
f1 *> f2 = f2

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f [] = pure []
mapA f (a:as) = (:) <$> (f a) <*> (mapA f as)

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (a:as) = (:) <$> a <*> (sequenceA' as)

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA 0 a = pure []
replicateA n a = (:) <$> a <*> (replicateA (n - 1) a)
