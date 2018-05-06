{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import qualified Data.Char as C

newtype Score = Score Int
  deriving (Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

lettersToPoints = [('A',1),('B',3),('C',3),('D',2)
                  ,('E',1),('F',4),('G',2),('H',4)
                  ,('I',1),('J',8),('K',5),('L',1)
                  ,('M',3),('N',1),('O',1),('P',3)
                  ,('Q',10),('R',1),('S',1),('T',1)
                  ,('U',1),('V',4),('W',4),('X',8)
                  ,('Y',4),('Z',10)]

getScore :: Score -> Int
getScore (Score x) = x

score :: Char -> Score
score c = case lookup (C.toUpper c) lettersToPoints of
  Just x -> Score x
  _ -> mempty

scoreString :: String -> Score
scoreString = foldr (\ch acc -> acc `mappend` (score ch))  mempty
