{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JoinList where

import Data.Monoid
import Sized
import Debug.Trace

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m j1 j2) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty j = j
(+++) j Empty = j
(+++) j1 j2 = Append (tag j1 <> tag j2) j1 j2

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ i (Single _ a) = if i == 0 then (Just a) else Nothing
indexJ i j@(Append m j1 j2)
  | i < 0 || i >= mb = Nothing
  | i < b1 = indexJ i j1
  | otherwise = indexJ (i - b1) j2
  where mb = (getSize . size)  m
        b1 = (getSize . size . tag) j1
        b2 = (getSize . size . tag) j2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


-- y = map (\i -> (indexJ i jl) == (jlToList jl !!? i)) [0 .. 7]

a = Append (Size 4)
    (Append (Size 3)
     (Single (Size 1) 'y')
     (Append (Size 2)
      (Single (Size 1) 'e')
      (Single (Size 1) 'a')))
    (Single (Size 1) 'h')

jl = (+++) a a


b = (Append (Size 2)
      (Single (Size 1) 'e')
      (Single (Size 1) 'a'))

c = (Single (Size 1) '!')

d = (+++) b c
e = (+++) d c

-- Exercise 3

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n Empty = Empty
dropJ n j@(Single _ _)
  | n <= 0 = j
dropJ n j@(Append m j1 j2)
  | n > mb = Empty
  | n > b1 = dropJ (n - b1) j2
  | otherwise = (+++) (dropJ n j1) j2
  where mb = (getSize . size) m
        b1 = (getSize . size . tag) j1
dropJ _ _ = Empty
