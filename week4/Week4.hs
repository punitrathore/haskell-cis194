module Week4 where

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 = (foldr (*) 1)  . (map (\x -> x - 2))  . (filter even)

fun2 :: Integer -> Integer
fun2 =  sum .
        filter even .
        takeWhile (/= 1) .
        iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: Eq a => [a] -> Tree a
foldTree xs = undefined

-- Exercise 3
xor :: [Bool] -> Bool
xor = odd .
      (foldr (\e acc -> if e then acc + 1 else acc) 0 )

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e acc -> (f e):acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base xs

-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
