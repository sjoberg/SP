{-# LANGUAGE TypeSynonymInstances #-}
module SP.Score.Argument where

import Data.Function
import Data.Hashable
import Data.HashMap.Lazy (keys, lookupDefault)
import Data.IntMap (intersectionWith, elems)
import Data.List (foldl', find, intersect, sortBy)
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import SP.ByteString
import SP.Cluster
import SP.Score.Math
import SP.Score.Score

-- | Degree 2 argument scores.
argumentScore :: (ArgumentCluster -> ObjectMap) 
              -> ArgumentCluster 
              -> ArgumentCluster 
              -> ArgumentRelation -> Incidence -> Incidence -> ArgumentScore
argumentScore m ai aj = ArgumentScore value Merge ai aj
  where
  value = 0.5 * scoreRelation ai aj + 0.5 * sobj

  sobj :: Double
  sobj = mean es
    where es = elems $ intersectionWith (\x y -> 1 - abs(x - y)) (m ai) (m aj)

-- | Score with respect to relation.
scoreRelation :: ArgumentCluster -> ArgumentCluster -> Double
scoreRelation ai@ArgumentCluster {} aj@ArgumentCluster {} = 
  let rels = (intersect `on` (keys . relMap)) ai aj
      get rel ak = lookupDefault 0 rel $ relMap ak
  in meanBy (\k -> 1 - abs (get k ai - get k aj)) rels
scoreRelation D2ArgumentCluster {acFst = aik, acSnd = ail}
         D2ArgumentCluster {acFst = ajk, acSnd = ajl}
  = (scoreRelation aik ajk + scoreRelation ail ajl) / 2
scoreRelation _ _ = 0

-- | Degree 2 argument scores. Currently a bit tailored to siblings.
d2ArgumentScore :: [ArgumentScore] -> ArgumentCluster -> ArgumentCluster 
                -> ArgumentRelation -> Incidence -> Incidence -> ArgumentScore
d2ArgumentScore ss ai aj = ArgumentScore value Merge ai aj 
  where value = hmean2 d1ScrVal d2ScrVal
        d2ScrVal = argScrVal 
                 $ argumentScore chdMap (acSnd ai) (acSnd aj) ChildArgument 0 0
        d1ScrVal = toVal $ find matches ss -- Find degree 1 scores.
          where matches s = a1 s == acFst ai && a2 s == acFst aj
                toVal (Just s) = argScrVal s
                toVal Nothing  = 0

-- | All argument scores valued above zero. 
argumentScores :: ObjectCluster -> ObjectCluster -> [ArgumentScore]
argumentScores oi oj = parScrs ++ chdScrs ++ sblScrs
  where
  -- Degree 1 argument scores.
  argScrs mapf = scores $ argumentScore mapf
  parScrs, chdScrs :: [ArgumentScore]
  parScrs = argScrs parMap pars ParentArgument
  chdScrs = argScrs chdMap chdn ChildArgument
  -- Degree 2 argument scores.
  argScrs2 = scores $ d2ArgumentScore parScrs

  sblScrs :: [ArgumentScore]
  sblScrs = argScrs2 sbls SiblingArgument
  -- Builds scores using score function scrFun, and fetches the argument
  -- clusters from the incidence lists accessed by iacc, filter for values > 0.
  scores scrFun incs argRel = 
    let scores = concatMap (\ih -> map (score ih) (incs oi)) (incs oj)
        score ih ik = scrFun (fst ih) (fst ik) argRel (snd ih) (snd ik)
    in filter (\score -> argScrVal score > 0) scores

       -- Fast and inaccurate. 
       -- filter (\s -> argScrVal s > 0) 
       --   $ zipWith (\ih ik -> scrFun (fst ih) (fst ik) (snd ih) (snd ik)) 
       --   (iacc oi) (iacc oj)

perm :: [a] -> [a] -> [(a,a)]
perm xs ys = concatMap (\x -> map (\y -> (x,y)) ys) xs

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

