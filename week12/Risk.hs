{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import qualified Data.List as L
import Debug.Trace
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

dv1 = DV 1
dv2 = DV 2
dv3 = DV 3

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = L.sortBy (flip compare)

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

bf1 = Battlefield {attackers=4, defenders=5}

nRolls :: Int -> Rand StdGen [DieValue]
nRolls n = replicateM n die

attackerWinCount :: (Ord a) => [a] -> [a] -> Int
attackerWinCount as ds = foldr (\win acc -> if win then(acc + 1) else (acc - 1)) 0 $
                         zipWith (\a d -> if a > d then True else False) as ds

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  let numARolls = if a > 3 then 3 else a - 1
      numDRolls = if d <= 2 then d else 2
  aRolls <- nRolls numARolls
  dRolls <- nRolls numDRolls
  let sortedARolls = sortDesc aRolls
      sortedDRolls = sortDesc dRolls
      aWinCount = attackerWinCount sortedARolls sortedDRolls
  if (aWinCount > 0)
    then return (Battlefield a (d - aWinCount))
    else return (Battlefield (a + aWinCount) d)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a d) = do
  if (a < 2 || d == 0)
    then return bf
    else battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM numBattles (invade bf) >>=
                 (\bfs -> return ((fromIntegral . length . attackWins) bfs / fromIntegral numBattles))
  where attackWins bfs = filter (\bf -> (defenders bf == 0)) bfs
        numBattles = 1000
