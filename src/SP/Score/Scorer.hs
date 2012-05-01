{-# LANGUAGE TupleSections #-}

module SP.Score.Scorer where

import qualified Control.Parallel.Strategies as P (rseq, parMap) 
import Data.Function (on)
import Data.IntMap (IntMap, difference, elems, intersectionWith)
import qualified Data.HashMap.Lazy as HashMap
import Data.List ((\\), foldl', partition)
import Data.List.Extras.Argmax (argmaxes)
import Data.Ord (comparing)
import SP.Cluster
import SP.Score.Argument 
import SP.Score.MaeIsaScorer
import SP.Score.Math
import SP.Score.Object
import SP.Score.Score

operatorScore :: Double -> (ObjectCluster,ObjectCluster) -> OperatorScore
operatorScore ta (oi,oj) = OperatorScore value Merge objScr magta
  where
  stop = null magta || (ocId oi `elem` objIds oj)
  value = if stop then 0 else tm
  objScr = objectScore oi oj 

  -- M_a,>0
  mag0 = bestArgumentScores oi oj --objArgScrs objScr
  -- M_a,>=ta & M_a,<ta
  (magta,malta) = partition (\s -> argScrVal s >= ta) mag0

  isumoi = sumBy i1 mag0
  isumoj = sumBy i2 mag0

  -- Merge score.
  tm = let tm1 = sumBy (\s -> i1 s * argScrVal s) mag0 / isumoi
           tm2 = sumBy (\s -> i2 s * argScrVal s) mag0 / isumoj
       in hmean2 tm1 tm2
  sm = let sm1 = sumBy (\s -> i1 s * sah s) magta / isumoi
           sm2 = sumBy (\s -> i2 s * sah s) magta / isumoj
       in hmean2 sm1 sm2


-- | Harmonic mean of argument score value and coefficient of variation for 
-- the argument cluster incidences.
sah :: ArgumentScore -> Double 
sah score = hmean2 (argScrVal score) (1 - ca2 (i1 score) (i2 score)) 

operatorScores :: Bool -> [[ObjectCluster]] -> [OperatorScore]
operatorScores isa os = P.parMap P.rseq score mergeTuples
  where score = if isa then isaOperatorScore paramSet . operatorScore 0.5
                       else operatorScore 0.5

        paramSet = ParamSet { tm = 0.5, ta = 0.5, tc = 0.5
                            , wm = 1, wa = 0.8, wc = 1
                            }

        mergeTuples = concatMap toTuples os
        toTuples []     = []
        toTuples (x:xs) = map (\y -> (x,y)) xs ++ toTuples xs

maxOperatorScores :: Bool -> [[ObjectCluster]] -> [OperatorScore]
maxOperatorScores isa = get . argmaxes opScrVal . operatorScores isa
  where get []             = []
        get (score:scores) = score:get (filter (isIndependentOf score) scores)
        s1 `isIndependentOf` s2 = ((/=) `on` o1 . objScr) s1 s2 &&
                                  ((/=) `on` o2 . objScr) s1 s2

