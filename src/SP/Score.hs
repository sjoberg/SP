module SP.Score where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List
import Data.List.Extras (argmaxes)
import Data.Ord (comparing)
import SP.Cluster

data Score = Score {scoreVal::Double, objScore::ObjScore, argScores::[ArgScore]} deriving (Show, Eq)
data ObjScore = ObjScore {objScoreVal::Double, oc1, oc2::ObjClr} deriving (Show, Eq)
data ArgScore = ArgScore {argScoreVal::Double, ac1, ac2::ArgClr} deriving (Show, Eq)

sOc1 = oc1 . objScore
sOc2 = oc2 . objScore

-- | Calculates the similarity between two argument clusters.
cmpac :: ArgClr -> ArgClr -> Double
cmpac (AdjArgClr _ _ xs _) (AdjArgClr _ _ ys _) = cmpAdjArgs xs ys
cmpac (RmtArgClr _ _ xs _) (RmtArgClr _ _ ys _) = cmpRmtArgs xs ys
cmpac _                     _                   = 0

-- | Calculates the similarity between two lists of adjacent arguments.
cmpAdjArgs :: [AdjArg] -> [AdjArg] -> Double
cmpAdjArgs xs ys = 0.5 * cmpFld rel xs ys + 0.5 * cmpFld obj xs ys

-- | Calculates the similarity between two lists of remote arguments.
cmpRmtArgs :: [RmtArg] -> [RmtArg] -> Double
cmpRmtArgs xs ys = scr intRel + scr rmtRel + scr intObj + scr rmtObj 
  where scr fld = 0.25 * cmpFld fld xs ys

-- | Calculates the similarity between two dependencies with respect to a field.
cmpFld :: (Eq b) => (a -> b) -> [a] -> [a] -> Double
cmpFld f xs ys = sum [1.0 - abs (freq f v xs - freq f v ys) | v <- uniques] / genericLength uniques
  where uniques = unique f xs ys

-- | The frequency of the value v in the dependencies xs.
freq :: (Eq b) => (a -> b) -> b -> [a] -> Double
freq f v xs = genericLength (filter (\x -> f x == v) xs) / genericLength xs

-- | The unique values of the property, transformed by f, in the dependencies in xs, ys.
unique :: (Eq b) => (a -> b) -> [a] -> [a] -> [b]
unique f xs ys = nub (map f xs `union` map f ys)

-- | Get the best combination of argument scores. Greedy search.
bestArgScores :: ObjClr -> ObjClr -> [ArgScore]
bestArgScores oc1 oc2 = takeBest ac1 ac2 $ sortBy (key argScoreVal) allScores
  where
    allScores = scrFor parArgClrs ++ scrFor chdArgClrs ++ scrFor sblArgClrs 
    scrFor fld = allArgScores (fld oc1) (fld oc2)

-- | Get the best combination of scores among the supplied scores.
-- | Input must be sorted descending by score value.
takeBest :: (Eq s, Eq c) => (s -> c) -> (s -> c) -> [s] -> [s]
takeBest _  _  []     = []
takeBest f1 f2 (s:ss) = s : takeBest f1 f2 (filter (indep f1 f2 s) ss)

-- | Returns true if the specified scores are independent.
-- | f1 and f2 are accessor methods for the clusters of the scores.
indep :: (Eq s, Eq c) => (s -> c) -> (s -> c) -> s -> s -> Bool
indep f1 f2 s1 s2 = null $ intersect [f1 s1, f2 s1] [f1 s2, f2 s2]

-- | Key for descending sort of scores.
key :: (a -> Double) -> a -> a -> Ordering
key f s1 s2 = comparing negate (f s1) (f s2)

-- | Get all argument scores.
allArgScores :: [ArgClr] -> [ArgClr] -> [ArgScore]
allArgScores xs ys = [ArgScore s x y | (s, x, y) <- zipWith (\x y -> (cmpac x y, x, y)) xs ys]

-- | Calculate the similarity between to object clusters.
scoreObjClr :: ObjClr -> ObjClr -> Score
scoreObjClr c1 c2 = 
  let argScores = bestArgScores c1 c2
      os = ObjScore (cmpFld form (ocParts c1) (ocParts c2)) c1 c2
      total = 0.5 * objScoreVal os + 0.5 * argScoreSum
      argScoreSum = sum (map argScoreVal argScores) / maxArgMerges
      maxArgMerges = max (acLen c1) (acLen c2)
      acLen c = len parArgClrs + len chdArgClrs + len sblArgClrs
        where
          len fld = genericLength (fld c)
  in Score total os argScores

-- | Calculate the best scores.
bestScores :: [ObjClr] -> [Score]
bestScores xs = takeBest (oc1 . objScore) (oc2 . objScore) $ argmaxes scoreVal (allScores xs)
    
-- | Calculate all scores. Parallel.
allScores :: [ObjClr] -> [Score]
allScores xs = parMap rpar f [(x,y) | x <- xs, y <- xs, x /= y]
  where f = uncurry scoreObjClr

