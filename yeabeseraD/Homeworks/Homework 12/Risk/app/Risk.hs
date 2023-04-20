{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Functor.Identity
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int
type Attacker  = Int
type Defender = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = return $ Battlefield atts' dfs'
  where atts            = attackers battlefield
        dfs             = defenders battlefield
        (atts', dfs')   = decideSubtract attacks defences (atts, dfs)
        attacks         = runIdentity $ evalRandT (fmap unDV <$> replicateM (allowedAttack atts) die) g
        defences        = runIdentity $ evalRandT (fmap unDV <$>  replicateM (allowedDefend dfs)  die) (runIdentity $ evalRandT getSplit g)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield atts defs)
  | atts < 2  = return bf
  | defs == 0 = return bf
  | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <-replicateM 1000 (invade bf)
  let wins = length $ filter id $ map (\bf -> attackers bf > 1) results
      prob = fromIntegral wins / 1000.0
  return prob

decideSubtract :: [Attacker] -> [Defender] -> (Attacker, Defender) -> (Attacker, Defender)
decideSubtract attacks defends (atts, defs)
  | null attacks && null defends  = (atts, defs)
  | null attacks                  = decideSubtract attacks restDefs  (atts - 1, defs)
  | null defends                  = decideSubtract restAtts defends  (atts, defs - 1)
  | head attacks' > head defends' = decideSubtract restAtts restDefs (atts, defs - 1)
  | otherwise                     = decideSubtract restAtts restDefs (atts - 1, defs)
  where attacks' = (reverse . sort) attacks
        defends' = (reverse . sort) defends
        restDefs = tail defends'
        restAtts = tail attacks'

allowedAttack :: Army -> Int
allowedAttack available | available < 3 =  available| available == 3 = 2 | otherwise = 3

allowedDefend :: Army -> Int
allowedDefend available | available <=2 =  available | otherwise = 3

battlefield = Battlefield {attackers=10, defenders = 5}
-- dfsArmy  = defenders battlefield
-- dfs      = replicate (allowed dfsArmy)  die
-- temp = fmap unDV <$> sequence dfs
-- defences = runIdentity $ evalRandT (fmap unDV <$>  replicateM 3 die)  (runIdentity $ evalRandT getSplit g)
-- attacks  = runIdentity $ evalRandT (fmap unDV <$>  replicateM 3 die) g
