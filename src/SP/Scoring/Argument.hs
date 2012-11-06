-- | Module for calculating argument scores.
module SP.Scoring.Argument (scores, meanError, filterMergers) where

import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap, elems, intersectionWith)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import SP.Cluster
import SP.Config
import SP.Scoring.Score
import SP.Math (hmean2, mean)

-- | Create scores for all argument clusters in the given object clusters.
scores :: ObjCluster -> ObjCluster -> [ArgScore]
scores x y = toScores (parents x) (parents y) ++ 
             toScores (children x) (children y) ++
             toScores (siblings x) (siblings y)

-- | Create a score for two argument clusters.
score :: ArgCluster -> ArgCluster -> ArgScore
score x y = ArgScore
    { argLeft = x
    , argRight = y
    , argScoreOp = Merge
    , argScoreValue = 1 - (objError + relError) / 2
    }
  where
    objError = if nearestNeighbour x || nearestNeighbour y
               then meanError objFrequency x y
               else hmean2 (meanError objFrequency x y) (meanError subObjFrequency x y)
    relError = meanError relFrequency x y

-- | Filter merger argument scores by score value threshold.
filterMergers :: Config -> [ArgScore] -> [ArgScore]
filterMergers cfg = filter ((>= tm cfg) . argScoreValue)

-- | Calculate mean error of a frequency.
meanError :: (Eq k, Hashable k) => (a -> HashMap k Double) -> a -> a -> Double
meanError f x y = (1 -) . mean . elems $ intersectionWith negAbsErr (f x) (f y)

-- | Negated absolute error.
negAbsErr :: Double -> Double -> Double
negAbsErr x y = 1 - abs (x - y)

-- | Pair, score, and align.
toScores :: [ArgCluster] -> [ArgCluster] -> [ArgScore]
toScores xs = align . map (uncurry score) . pair xs

-- | Greedy align of scores. Heuristically finds the best combination of scores. 
align :: [ArgScore] -> [ArgScore]
align = foldIndependent . sortByScore
  where 
    sortByScore = sortBy (comparing $ negate . argScoreValue)
    foldIndependent = foldl' (\r n -> if all (independent n) r then r else n:r) []

-- | Calculate combinations of argument clusters to score.
pair :: [ArgCluster] -> [ArgCluster] -> [(ArgCluster, ArgCluster)]
pair xs ys = concatMap (\x -> map (\y -> (x,y)) ys) xs
