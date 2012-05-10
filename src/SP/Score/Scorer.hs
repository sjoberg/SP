{-# LANGUAGE TupleSections #-}

module SP.Score.Scorer where

import Debug.Trace
import Control.Arrow (second)
import Control.Parallel.Strategies as P
import Control.Parallel
import Data.Function (on)
import Data.IntMap (IntMap, difference, elems, intersectionWith)
import qualified Data.HashMap.Lazy as HashMap
import Data.List ((\\), foldl', partition)
import Data.List.Extras.Argmax (argmaxes)
import Data.Ord (comparing)
import SP.Cluster
import SP.Score.Argument 
import SP.Score.Math
import SP.Score.Object
import SP.Score.Score
import SP.Debug

-- | Evaluate scores in parallel.
operatorScores :: ParamSet -> [[ObjectCluster]] -> [OperatorScore]
operatorScores paramSet = let score = operatorScore paramSet
                              mapP f = withStrategy (parBuffer 80 rseq) . map f
                          in mapP score . concatMap toTuples

-- | Convert a list of object clusters to pairs of object clusters.
toTuples :: [ObjectCluster] -> [(ObjectCluster, ObjectCluster)]
toTuples []     = []
toTuples (x:xs) = map (\y -> (x,y)) xs ++ toTuples xs

-- Create an operator score.
operatorScore :: ParamSet -> (ObjectCluster,ObjectCluster) -> OperatorScore
operatorScore paramSet@ParamSet {wm = wm, wa = wa, wc = wc, tm = tm} (oi,oj) = 
  let -- Whether oi and oj are neighbours.
      areNeighbours = ocId oi `elem` objIds oj
      
      -- Object score and argument scores.
      objScr = objectScore oi oj 
      argScrs = bestArgumentScores oi oj
 
      -- Merge and ISA argument Scores.
      isaArgScrs = concatMap (isaArgumentScores paramSet) argScrs
      scrsFor Merge = filter ((>= tm) . argScrVal) argScrs 
      scrsFor op    = filter ((== op) . argScrOp) isaArgScrs

      -- Incidence sums.
      iSum1 = incidenceSum oi
      iSum2 = incidenceSum oj

      -- Weight mark and tuple of scores and mark.
      -- fm is a function that calculates an unweighted mark for scores.
      mark fm w op = let scrs = scrsFor op 
                     in (scrs, showAltScores paramSet op . prior w . fm $ scrs)

      -- Marks.
      partMark = partScrVal objScr

      mrgMark = mark (matchMark2 iSum1 iSum2) wm Merge
      absMark = mark (matchMark2 iSum1 iSum2) wa Abstract
      chdMark = mark (matchMark1 iSum1 i1) wc Child
      parMark = mark (matchMark1 iSum2 i2) wc Parent

      -- Elected scores, mark value and operation.
      (elScrs,elMark) = if useIsa paramSet && partMark < 0.15
                          then argmax snd [mrgMark, absMark, chdMark, parMark]
                          else mrgMark
      elOp = argScrOp $ head elScrs
  in OperatorScore elMark elOp objScr elScrs
 
-- | Match mark.
matchMark1 :: Double -> (ArgumentScore -> Double) -> [ArgumentScore] -> Double
matchMark1 iSum fi = sumBy $ \s -> argScrVal s * fi s / iSum

matchMark2 :: Double -> Double -> [ArgumentScore] -> Double
matchMark2 iSum1 iSum2 = let f, g :: ArgumentScore -> Double
                             f s = argScrVal s * i1 s / iSum1
                             g s = argScrVal s * i2 s / iSum2
                             sum = foldl' (\(t,u) s -> (t + f s, u + g s)) (0,0)
                         in uncurry hmean2 . sum 

-- | Helper function to show alternative scores.
showAltScores :: ParamSet -> Operator -> Double -> Double
showAltScores params op = if printMarks params then vtrace (show op) else id

-- | Transform an argument score to an argument score with ISA support.
isaArgumentScores :: ParamSet -> ArgumentScore -> [ArgumentScore]
isaArgumentScores ParamSet {ta = ta, tc = tc} score = 
  let relMark = scoreRelation (a1 score) (a2 score)

      -- Maps of object cluster references
      m = objRefMap score $ a1 score
      n = objRefMap score $ a2 score

      -- Errors
      absErr x y   = abs $ x - y
      matchErrs    = elems $ intersectionWith absErr m n
      noMatchErrs1 = elems $ difference m n
      noMatchErrs2 = elems $ difference n m

      -- Marks

      -- Transform errors to marks
      mark errs = (1 - mean errs + relMark) / 2
      -- Arg. cluster 1 is parent to arg. cluster 2
      parMark = mark $ matchErrs ++ noMatchErrs2
      -- Arg. cluster 2 is parent to arg. cluster 1
      chdMark = mark $ matchErrs ++ noMatchErrs1
      -- Arg. clusters 1 and 2 have a common cluster
      absMark = mark matchErrs

  in [score {argScrVal = parMark, argScrOp = Parent}   | parMark >= tc]
  ++ [score {argScrVal = chdMark, argScrOp = Child}    | chdMark >= tc]
  ++ [score {argScrVal = absMark, argScrOp = Abstract} | absMark >= ta]

maxOperatorScores :: ParamSet -> [[ObjectCluster]] -> [OperatorScore]
maxOperatorScores paramSet = get . argmaxes opScrVal . operatorScores paramSet
  where get []             = []
        get (scr : scrs) = scr : get (filter (isIndependentOf scr) scrs)
        s1 `isIndependentOf` s2 = ((/=) `on` o1 . objScr) s1 s2 &&
                                  ((/=) `on` o2 . objScr) s1 s2

