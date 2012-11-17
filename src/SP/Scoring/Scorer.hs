-- | Scoring.
module SP.Scoring.Scorer where

import Control.Parallel.Strategies (parMap, rseq)
import Data.HashMap.Lazy (keys)
import Data.HashSet (HashSet, empty)
import Data.List (foldl')
import Data.List.Extras.Argmax (argmaxes)
import SP.Cluster
import SP.Config (Config, tsyn, useHyponymys)
import SP.Scoring.Argument (filterMergers, scores)
import SP.Scoring.Hyponymy (frequencySum,hyponymize,toScoreSet)
import SP.Scoring.Score
import SP.Scoring.Part (scoreParts)
import SP.Scoring.UnqualifiedSet (sampleSet)

-- | Greedy align of scores. Heuristically finds the best combination of scores.
-- Do not pass empty scores.
alignBest :: [Score] -> [Score]
alignBest = foldIndependent . argmaxes scoreValue
  where
    foldIndependent = foldl' (\r n -> if all (unrelated n) r then n:r else r) []
    unrelated x y = not $ objLeft x `areRelated` objLeft y ||
                          objLeft x `areRelated` objRight y ||
                          objRight x `areRelated` objLeft y ||
                          objRight x `areRelated` objRight y

-- | Evaluates scores in parallel.
mkScores :: Config -> [[ObjCluster]] -> [Score]
mkScores cfg xs = parMap rseq (score cfg unqlfSet) . concatMap pair $ xs
  where
    unqlfSet = if useHyponymys cfg then sampleSet 1 (concat xs) else empty

-- | Create pairs of object clusters to score.
pair :: [ObjCluster] -> [(ObjCluster, ObjCluster)]
pair []     = []
pair (x:xs) = [(x,y) | y <- xs, not (areRelated x y)] ++ pair xs

-- | True if the supplied object clusters are related.
areRelated :: ObjCluster -> ObjCluster -> Bool
areRelated x y = any (== objClusterId x) yHyponymys -- (yObjs ++ ySubObjs) ||
                 --any (== objClusterId y) (xObjs ++ xSubObjs) 
                 -- Hyponymys exist symmetrically, no need to check hyponyms twice.
  where
    {- propKeys f c = concatMap (keys . f) (children c) --(allArgClusters c) 
    xObjs = propKeys objFrequency x
    yObjs = propKeys objFrequency y
    xSubObjs = propKeys subObjFrequency x
    ySubObjs = propKeys subObjFrequency y -}
    yHyponymys = map objClusterId (hypernyms y ++ hyponyms y)

-- | Create a score between two object clusters.
score :: Config -> HashSet Int -> (ObjCluster,ObjCluster) -> Score
score cfg unqlfSet (x,y) = if useHyponymys cfg && not partsSimilar 
                           then electedScore
                           else mergeScore
  where
    -- True if part similarity is above threshold.
    partsSimilar = scoreParts x y >= tsyn cfg
    allArgScores = scores x y -- All argument scores.
    electedScore = hyponymize cfg unqlfSet 
                   mergeScore {argScores = allArgScores}
    -- Merge points, merge argument scores and merge score.
    mergeScores = filterMergers cfg allArgScores
    ((_,mergePoints),_) = toScoreSet (frequencySum x) (frequencySum y) (Merge, mergeScores)
    mergeScore = Score { objLeft = x
                       , objRight = y
                       , scoreOp = Merge
                       , scoreValue = mergePoints
                       , argScores = mergeScores
                       }

                       