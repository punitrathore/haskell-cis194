-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = (n `mod` 10):(toDigitsRev (quot n 10))

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = x:y*2:(doubleEveryOther xs)

reduceToSingleDigit n
  | n < 10 = n
  | otherwise = (n `mod` 10) + reduceToSingleDigit (quot n 10)

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 $ map reduceToSingleDigit xs

-- Exercise 4
validate :: Integer -> Bool
validate n = ((sumDigits $ doubleEveryOther (toDigits n)) `mod` 10) == 0


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 p3 = [(p1, p2)]
hanoi n p1 p2 p3 = (hanoi (n-1) p1 p3 p2) ++ [(p1, p2)] ++ (hanoi (n-1) p3 p2 p1)
