{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Party where

import Data.Tree
import Debug.Trace
import Employee

-- Exercise 1

instance Monoid Fun where
  mempty = 0
  mappend = (+)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 `mappend` e2) (f1 `mappend` f2)

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL el f) = GL (emp:el) (mappend f (empFun emp))

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2)
  | f1 >= f2 = g1
  | otherwise = g2

-- Exercise 2

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold b f (Node a ts) = f a (map (treeFold b f) ts)

treeFun = treeFold 0 (\e r -> (empFun e) + (foldr (+) 0 r))


-- Exercise 3

maxGuestList :: [GuestList] -> GuestList
maxGuestList [] = mempty
maxGuestList gl = maximum gl

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss, withoutBoss)
  where bestWithSubBoss = map fst gls
        bestWithoutSubBoss = map snd gls
        withoutBoss = maxGuestList bestWithSubBoss
        withBoss = maxGuestList (map (glCons boss) bestWithoutSubBoss)

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry max $ treeFold (mempty, mempty) nextLevel tree

maxFunReport :: Tree Employee -> String
maxFunReport emps = let (GL guestListEmps f) = maxFun emps
                        names = unlines $ map empName guestListEmps
                    in
                      "Total fun: " ++ (show f) ++ "\n" ++ names

main :: IO ()
main = do
  empsString <- readFile "company.txt"
  let emps = read empsString
  (putStrLn . maxFunReport) emps
