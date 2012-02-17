{-# LANGUAGE TypeSynonymInstances #-}
module SP.Score.Argument where

import Data.Function
import Data.Hashable
import Data.HashMap.Lazy (keys, lookupDefault)
import Data.IntMap (intersectionWith, elems)
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import SP.ByteString
import SP.Cluster
import SP.Score.Math
import SP.Score.Score
import Data.List.Stream -- (find, foldl', intersect, sortBy)
import Prelude hiding (any,take,head,null,sum,(++),filter,foldr,length)

-- | Degree 2 argument scores.
argumentScore :: (ArgumentCluster -> ObjectMap) -> ArgumentCluster -> 
                 ArgumentCluster -> (Incidence -> Incidence -> ArgumentScore)
argumentScore m ai aj = ArgumentScore value ai aj
  where
  value = (srel + sobj) / 2

  srel, sobj:: Double
  srel = meanBy (\k -> 1 - abs (get k ai - get k aj)) ks
    where ks = (intersect `on` (keys.relMap)) ai aj
          get k ak = lookupDefault 0 k $ relMap ak
  --srel | null ks   = 0
  --     | otherwise = --1 - 1 / (fromIntegral.length) ks * 
                     --foldl' (\r k -> r + abs (get k ai - get k aj)) 0 ks

  sobj = mean es -- | null es   = 0
    where es = elems $ intersectionWith (\x y -> 1 - abs(x - y)) (m ai) (m aj)
       -- | otherwise = --1 - 1 / cardinality es * sum es
       -- 1 - mean es

-- | Degree 2 argument scores. Currently a bit tailored to siblings.
d2ArgumentScore :: [ArgumentScore] -> ArgumentCluster -> ArgumentCluster -> 
                   (Incidence -> Incidence -> ArgumentScore)
d2ArgumentScore ss ai aj = ArgumentScore value ai aj 
  where value = hmean2 d1ScrVal d2ScrVal
        d2ScrVal = argScrVal $ argumentScore chdMap (acSnd ai) (acSnd aj) 0 0
        d1ScrVal = toVal $ find matches ss -- Find degree 1 scores.
          where matches s = a1 s == acFst ai && a2 s == acFst aj
                toVal (Just s) = argScrVal s
                toVal Nothing  = 0

-- | All argument scores valued above zero. 
argumentScores :: ObjectCluster -> ObjectCluster -> [ArgumentScore]
argumentScores oi oj = parScrs ++ chdScrs ++ sblScrs
  where
  parScrs = as parMap pars; chdScrs = as chdMap chdn; sblScrs = as2 sbls
  -- Degree 1 argument scores.
  as m = scores $ argumentScore m
  -- Degree 2 argument scores.
  as2 = scores $ d2ArgumentScore parScrs
  -- Builds scores using score function scrFun, and fetches the argument
  -- clusters from the incidence lists accessed by iacc, filter for values > 0.
  scores scrFun iacc = [score | ih <- iacc oi, ik <- iacc oj, 
                        let score = scrFun (fst ih) (fst ik) (snd ih) (snd ik),
                        argScrVal score > 0]

-- | Greedy search for the best combination of argument scores.
bestArgumentScores :: ObjectCluster -> ObjectCluster -> [ArgumentScore]
bestArgumentScores oi oj = align sorted
  where
  -- Descending sort of all argument scores valued above 0.
  sorted = sortBy (comparing (negate.argScrVal)) (argumentScores oi oj)
--align = foldr (\n r -> n:filter (isIndependentOf n) r) []
  align = let f r n | any (isDependentOn n) r = r
                    | otherwise               = n:r
          in foldl' f []
       
--3 funkar
--align :: [ArgumentScore] -> [ArgumentScore]
--align = fst.foldl' f ([], Set.empty)
--  where f :: ([ArgumentScore], Set.Set ArgumentCluster) -> ArgumentScore -> 
--             ([ArgumentScore], Set.Set ArgumentCluster)
--        f (as,s) n | Set.member (a1 n) s || Set.member (a2 n) s = (as,s)
--                   | otherwise = (n:as, Set.insert (a1 n).Set.insert (a2 n) $ s)
--1 funkar
--align (score:scores) = score:align (filter (isIndependentOf score) scores)
--align []             = []
--2 funkar ej
--align (score:scores) = foldr construct [score] scores
--  where construct n r = if n `isIndependentOf` head r then n:r else r
--align []             = []

-- | Independence of argument scores.
isIndependentOf :: ArgumentScore -> ArgumentScore -> Bool
isIndependentOf ArgumentScore {a1 = ai, a2 = aj} 
                ArgumentScore {a1 = ak, a2 = al} = ai /= ak && aj /= al

-- | Independence of argument scores.
isDependentOn :: ArgumentScore -> ArgumentScore -> Bool
isDependentOn ArgumentScore {a1 = ai, a2 = aj} 
              ArgumentScore {a1 = ak, a2 = al} = ai == ak || aj == al

instance Ord ArgumentCluster where
  (<=) = (<=) `on` hash

