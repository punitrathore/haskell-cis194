{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Parser
import qualified StackVM as StackVM
import qualified Data.Map as M
import Data.Maybe

-- Exercise 1
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
           deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (Mul exp1 exp2) = (eval exp1) * (eval exp2)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Just exp -> Just (eval exp)
  Nothing -> Nothing

-- Exercise 3
class (Expr a) where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance (Expr ExprT) where
  lit = Lit
  mul = Mul
  add = Add

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance (Expr Integer) where
  lit = id
  mul = (*)
  add = (+)

instance (Expr Bool) where
  lit x = x > 0
  add = (||)
  mul = (&&)

instance (Expr MinMax) where
  lit = MinMax
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)

instance (Expr Mod7) where
  lit x = Mod7 (x `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  mul x y = x ++ y ++ [StackVM.Mul]
  add x y = x ++ y ++ [StackVM.Add]


compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = Lit2 Integer
              | Var2 String
              | Add2 VarExprT VarExprT
              | Mul2 VarExprT VarExprT
              deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var2

instance Expr VarExprT where
  lit = Lit2
  mul = Mul2
  add = Add2

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

applyOperatorToVarExp op m x y = case isNothing(x m) || isNothing(y m) of
                                 True -> Nothing
                                 False -> Just(op (fromJust(x m)) (fromJust(y m)))

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \m -> Just x
  mul x y = \m -> applyOperatorToVarExp (*) m x y
  add x y = \m -> applyOperatorToVarExp (+) m x y

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

--  withVars [("x", 6), ("y", 3)] $ mul (add (lit 3) (var "x")) (var "y")
