module Score (Score, ObjScore, ArgScore, allScores, bestScores) where

import Cluster
import Data.List

data Score = Score {scoreVal::Double, objScore::ObjScore, argScores::[ArgScore]} deriving (Show, Eq)
data ObjScore = ObjScore {objScoreVal::Double, oc1::ObjCluster, oc2::ObjCluster} deriving (Show, Eq)
data ArgScore = ArgScore {argScoreVal::Double, ac1::ArgCluster, ac2::ArgCluster} deriving (Show, Eq)

-- Calculates the similarity between two argument clusters.
cmpac :: ArgCluster -> ArgCluster -> Double
cmpac (SibCluster _ xs) (SibCluster _ ys) = cmpDeps xs ys
cmpac (ParCluster _ xs) (ParCluster _ ys) = cmpDeps xs ys
cmpac (ChdCluster _ p1) (ChdCluster _ p2) = 0.5 * cmpDeps xs ys + 0.5 * cmpDeps zs ws      
      where
        uzp1 = unzip p1
        uzp2 = unzip p2
        xs = fst uzp1
        ys = fst uzp2
        zs = snd uzp1
        ws = snd uzp2
cmpac _ _ = 0.0

-- Calculates the similarity between two dependencies.
cmpDeps :: [Dep] -> [Dep] -> Double
cmpDeps xs ys = 0.5 * cmpProp rel xs ys + 0.5 * cmpProp obj xs ys

-- Calculates the similarity between two dependencies with respect to a property.
cmpProp :: (Eq b) => (a -> b) -> [a] -> [a] -> Double
cmpProp f xs ys = sum [1.0 - (abs $ freq f v xs - freq f v ys) | v <- uniques] / genericLength uniques
                    where uniques = unique f xs ys

-- The frequency of the value v in the dependencies xs.
freq :: (Eq b) => (a -> b) -> b -> [a] -> Double
freq f v xs = genericLength(filter (\x -> f x == v) xs) / genericLength xs

-- The unique values of the property, transformed by f, in the dependencies in xs, ys.
unique :: (Eq b) => (a -> b) -> [a] -> [a] -> [b]
unique f xs ys = nub (union (map f xs) (map f ys))

-- Get the best combination of argument merge scores. Greedy search.
bestArgScores :: ObjCluster -> ObjCluster -> [ArgScore]
bestArgScores c1 c2 = takeBest ac1 ac2 $ sortBy (key argScoreVal) $ allArgScores (argClusters c1) (argClusters c2)

-- Get the best combination of scores among the supplied scores.
-- Input must be sorted descending by score value.
takeBest :: (Eq s, Eq c) => (s -> c) -> (s -> c) -> [s] -> [s]
takeBest _  _  []     = []
takeBest f1 f2 (s:ss) = s : takeBest f1 f2 (filter (indep f1 f2 s) ss)

-- Returns true if the specified scores are independent.
-- f1 and f2 is accessor methods for the clusters of the scores.
indep :: (Eq s, Eq c) => (s -> c) -> (s -> c) -> s -> s -> Bool
indep f1 f2 s1 s2 = null $ intersect [f1 s1, f2 s1] [f1 s2, f2 s2]

-- Key for descending sort of scores.
key :: (a -> Double) -> a -> a -> Ordering
key f s1 s2 = compare (negate $ f s1) (negate $ f s2)

-- Get all argument merges.
allArgScores :: [ArgCluster] -> [ArgCluster] -> [ArgScore]
allArgScores xs ys = [ArgScore (cmpac x y) x y | x <- xs, y <- ys, cmpac x y > 0]

-- Calculate the similarity between to object clusters.
cmpObjCluster :: ObjCluster -> ObjCluster -> Score
cmpObjCluster c1 c2 = let argScores = bestArgScores c1 c2
                          os = ObjScore (cmpProp form (parts c1) (parts c2)) c1 c2
                          total = 0.5 * objScoreVal os + 0.5 * sum [argScoreVal as | as <- argScores]
                          in Score total os argScores

-- Calculate the best scores.
bestScores :: [ObjCluster] -> [Score]
bestScores xs = takeBest (oc1 . objScore) (oc2 . objScore) $ sortBy (key scoreVal) (allScores xs)

-- Calculate all scores.
allScores :: [ObjCluster] -> [Score]
allScores xs = [cmpObjCluster x y | x <- xs , y <- xs, x /= y]

