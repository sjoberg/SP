module SP.Score.Scorer where

import Control.Parallel.Strategies as P (rseq, parMap) 
import Data.Function (on)
--import Data.List --(partition)
import Data.List.Extras.Argmax (argmaxes,argmax)
import Data.Ord (comparing)
import SP.Cluster
import SP.Score.Math
import SP.Score.Object
import SP.Score.Score

import Data.List.Stream
import Prelude hiding (any, concatMap, elem, filter, map, null, (++), sum)

operatorScore :: Double -> (ObjectCluster,ObjectCluster) -> OperatorScore
operatorScore ta (oi,oj) = OperatorScore value Merge objScr
  where
  stop = null magta || (ocId oi `elem` objIds oj)
  value = if stop then 0 else tm
  objScr = objectScore oi oj 

  -- M_a,>0
  mag0 = objArgScrs objScr
  -- M_a,>=ta & M_a,<ta
  (magta,malta) = partition (\s -> argScrVal s >= ta) mag0

  isumoi = sumBy i1 mag0
  isumoj = sumBy i2 mag0

  sumBy f = foldl' (\t sa -> t + f sa) 0

  sah s = hmean2 (argScrVal s) (1 - (ca2 (i1 s) (i2 s))) 

--its ok = pars ok ++ chdn ok ++ sbls ok

--u1 = filter fu $ its oi 
--u2 = filter fu $ its oj
--fu it = not.any (\m -> a1 m == fst it || a2 m == fst it) $ mag0
    
--iusumoi = sumBy snd u1
--iusumoj = sumBy snd u2
  
  -- TODO Fold and calculate all at once.
--tu1 = iusumoi + sumBy (\s -> i1 s * argScrVal s) malta
--tu2 = iusumoj + sumBy (\s -> i2 s * argScrVal s) malta
--su1 = iusumoi + sumBy (\s -> i1 s * sah s) malta
--su2 = iusumoj + sumBy (\s -> i2 s * sah s) malta
--t'm1 = sumBy (\s -> i1 s * argScrVal s) magta
--t'm2 = sumBy (\s -> i2 s * argScrVal s) magta
--s'm1 = sumBy (\s -> i1 s * sah s) magta
--s'm2 = sumBy (\s -> i2 s * sah s) magta

--imsumoi = sumBy i1 magta
--imsumoj = sumBy i2 magta
--imusumoi = isumoi - imsumoi
--imusumoj = isumoj - imsumoj
--
--iasumoi = isumoi + iusumoi
--iasumoj = isumoj + iusumoj

  -- Merge score.
  tm = let tm1 = sumBy (\s -> i1 s * argScrVal s) mag0 / isumoi
           tm2 = sumBy (\s -> i2 s * argScrVal s) mag0 / isumoj
       in hmean2 tm1 tm2
  sm = let sm1 = sumBy (\s -> i1 s * sah s) magta / isumoi
           sm2 = sumBy (\s -> i2 s * sah s) magta / isumoj
       in hmean2 sm1 sm2
  
  -- Abstract.
--ta = let ta1 = (tu1 + t'm1) / 2 / iasumoi + (tu2 + t'm2) / 2 / iasumoj
--         ta2 = 1 - ca4 isumoi iusumoj imsumoi imsumoj
--     in hmean2 ta1 ta2
--sa = let sa1 = (su1 + s'm1) / 2 / iasumoi + (su2 + s'm2) / 2 / iasumoj
--         sa2 = 1 - ca4 isumoi iusumoj imsumoi imsumoj
--     in hmean2 sa1 sa2
  
  -- Parent / child score.
--imsum = imsumoi + imsumoj
--di = ca2 imsum iusumoi - ca2 imsum iusumoj
--tcp,scp,imsum,di,sa,ta,tm,sm :: Double
--tcp = let tcp1 = (tu1 + t'm1) / 2 + (tu2 + t'm2) / 2
--          tcp2 = abs di
--      in hmean2 tcp1 tcp2
--scp = let scp1 = (su1 + s'm1) / 2 + (su2 + s'm2) / 2
--          scp2 = abs di
--      in hmean2 scp1 scp2
  
--(typeScore,bestScore,op) = argmax (\(t,_,_) -> t) scores
--scores = (tm,sm,Merge):(ta,sa,Abstract):(chdScrTuple ++ parScrTuple)
--chdScrTuple | di > 0 && imsum > imusumoi + iusumoi = [(tcp,scp,Child)]
--            | otherwise                            = []
--parScrTuple | di < 0 && imsum > imusumoj + iusumoj = [(tcp,scp,Parent)]
--            | otherwise                            = []

operatorScores :: [[ObjectCluster]] -> [OperatorScore]
operatorScores os = P.parMap rseq (operatorScore 0.5) mergeTuples
  where mergeTuples = concatMap toTuples os
        toTuples []     = []
        toTuples (x:xs) = map (\y -> (x,y)) xs ++ toTuples xs

maxOperatorScores :: Double -> [[ObjectCluster]] -> [OperatorScore]
maxOperatorScores top = get.argmaxes opScrVal.operatorScores
  where f = filter (\s -> opScrVal s > top)
        get []             = []
        get (score:scores) = score:get (filter (isIndependentOf score) scores)
        s1 `isIndependentOf` s2 = ((/=) `on` o1.objScr) s1 s2 &&
                                  ((/=) `on` o2.objScr) s1 s2

