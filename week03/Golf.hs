module Golf where

-- Exercise 1 : Hopscotch
-- skips is chopping one element from the list by using pattern matching, and cons-ing the current list with the recursive call of skips of the remaining
skips :: [a] -> [[a]]
skips (x:[]) = [[x]]
skips l@(x:xs) = l:skips(xs)

-- Exercise 2 : Local Maxima
-- localMaxima is using pattern matching to extract the first 3 elements from the list, and applying the local maxima check to the 2nd element. If it is true, then it adds that result to the tail of the list.
localMaxima :: [Integer] -> [Integer]
localMaxima (x:[]) = []
localMaxima (x:y:[]) = []
localMaxima (x:tail@(y:z:l)) = case (y > x && y > z) of
  True -> y:(localMaxima tail)
  False -> localMaxima tail


-- Exercise 3 : Histogram
--
slice :: Int -> Int -> [Integer] -> [Integer]
slice from to xs = take (to - from + 1) (drop from xs)

incrementHist :: [Integer] -> Int -> [Integer]
incrementHist acc e = (slice 0 (e - 1) acc) ++  [(acc !! e) + 1] ++ (slice (e+1) 10 acc)

makeHistogram :: [Int] -> [Integer]
makeHistogram xs = foldl incrementHist (take 10 $ repeat 0) xs

histChar hist ln pos = case (hist !! pos) >= ln of
  True -> '*'
  False -> ' '

histLine hist ln = map (histChar hist ln) [0 .. 9] ++ "\n"

prettyHistogram hist = (concat $ (map (histLine hist) (reverse [1 .. maxOccurences]))) ++ "==========\n0123456789\n"
  where maxOccurences = maximum hist

histogram :: [Int] -> String
histogram xs = prettyHistogram $ makeHistogram xs
