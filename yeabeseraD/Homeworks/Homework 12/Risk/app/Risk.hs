{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Traversable (Traversable(..))

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

instance Traversable (RandT g) where 
  traverse f (RandT m) = RandT $ do 
    x <- m
    lift $ traverse f x

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }



battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = undefined
  -- Battlefield $ maximum $ unDV <$> atts
  where attsArmy   = attackers battlefield
        dfsArmy    = defenders battlefield
        atts       = replicate (allowedAttack attsArmy) die
        dfs        = replicate (allowedDefend dfsArmy)  die
        sortedDfs  = sort <$> randsToInts dfs
        sortedAtts = sort <$> randsToInts atts

randsToInts :: [Rand StdGen DieValue] -> Rand StdGen [Int]
randsToInts rands = fmap unDV <$> sequence rands

type Attacker = Int
type Defender = Int

decideWinner ::Rand StdGen [Int] -> Rand StdGen [Int] -> (Attacker , Defender) -> (Attacker, Defender)
decideWinner atts dfs (attackers, defenders)
  | null attsModified && null dfsModified = (attackers, defenders)
  | null attsModified                     = decideWinner atts restDfs (attackers - 1, defenders)
  | null dfsModified                      = decideWinner restAtts dfs (attackers, defenders)
  | (head <$> atts) > (head <$> dfs)      = decideWinner restAtts restDfs (attackers, defenders - 1)
  | otherwise                             = decideWinner restAtts restDfs (attackers - 1, defenders)
  where restDfs = tail <$> dfs
        restAtts = tail <$> atts
        attsModified = undefined

        dfsModified :: [Rand StdGen Int]
        dfsModified  = do
          xs <- dfs
          let temp = map return xs
          return temp

-- battlefield = Battlefield {attackers=10, defenders = 5}
-- dfsArmy  = defenders battlefield
-- dfs      = replicate (allowed dfsArmy)  die
-- temp = fmap unDV <$> sequence dfs

allowedAttack :: Army -> Int
allowedAttack available | available < 3 =  available| available == 3 = 2 | otherwise = 3

allowedDefend :: Army -> Int
allowedDefend available | available <=2 =  available | otherwise = 3
