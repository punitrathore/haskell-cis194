{- CIS 194 HW 10
   due Monday, 1 April
-}
{-# LANGUAGE FlexibleContexts #-}

module AParser where

import           Control.Applicative

import           Data.Char
import           Data.Maybe

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1

first :: (a -> b ) -> (a,c) -> (b, c)
first f (a,c) = (f a, c)


-- fmap :: (a -> b) -> Parser a -> Parser b
-- a :: String -> Maybe (a, String)
-- b :: String -> Maybe (b, String)
-- fmap (String -> Maybe (a, String) -> String -> Maybe (b, String) -> Parser (String -> Maybe(a, String)) -> Parser (String -> Maybe(b, String))

instance Functor Parser where
  fmap g (Parser f) = Parser (\s -> case (f s) of
                                      Just (a, s') -> Just (g a, s')
                                      otherwise -> Nothing)

-- Exercise 2
-- <*> :: Parser (a -> b) -> Parser a -> Parser b
-- a :: String -> Maybe (a, String)
-- b :: String -> Maybe (b, String)
-- <*> :: Parser ((String -> Maybe (a -> b, String)) -> Parser (String -> Maybe (a, String)) -> Parser (String -> Maybe (b, String))
instance Applicative Parser where
  pure a = Parser (\s -> Just(a, s))
  (Parser f) <*> (Parser p) = Parser (\s -> case f s of
                                              Nothing -> Nothing
                                              Just (f, sf) -> fmap (first f) (p sf))


type Name = String
data Employee = Emp { name :: Name, phone :: String } deriving (Show)

parseName :: Parser Name
parseName = Parser (\s -> Just(s::Name, s))
parsePhone :: Parser String
parsePhone = Parser (\s -> Just(s, s))

empParser = Emp <$> parseName <*> parsePhone :: Parser Employee

f = runParser empParser "Punit"

-- Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ =  const () <$> abParser

g (a, b, c) = [a,b]

intPair = f <$> posInt <*> char ' ' <*> posInt
  where f = (\a b c -> [a,c])

-- abParser_ :: Parser ()
-- abParser_ = Parser f
--   where
--     f "" = Nothing
--     f ('a':'b':xs) = Just((), xs)
--     f (x:xs) = f xs

instance Alternative Parser where
  empty = Parser (\x -> Nothing)
  p1 <|> p2 = Parser (\s -> case runParser p1 s of
                             Nothing -> runParser p2 s
                             res@(Just _) -> res)

posInt_ = const () <$> posInt
isUpper_ = const () <$> (satisfy isUpper)

intOrUppercase :: Parser ()
intOrUppercase = posInt_ <|> isUpper_
