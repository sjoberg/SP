-- | Scoring.
module SP.Scoring.Scorer where

import Control.Parallel.Strategies (parMap, rseq)
import Data.HashMap.Lazy (keys)
import SP.Cluster
import SP.Config (Config, tsyn, useHyponymys)
import SP.Scoring.Argument (filterMergers, scores)
import SP.Scoring.Hyponymy (frequencySum,hyponymize,toScoreSet)
import SP.Scoring.Score
import SP.Scoring.Part (scoreParts)
import SP.Scoring.UnqualifiedSet (sampleSet)
import Data.HashSet (HashSet, empty)

-- | Evaluates scores in parallel.
mkScores :: Config -> [ObjCluster] -> [Score]
mkScores cfg xs = parMap rseq (uncurry $ score cfg unqlfSet) . pair $ xs
  where
    unqlfSet = if useHyponymys cfg then sampleSet 1 xs else empty

-- | Create pairs of object clusters to score.
pair :: [ObjCluster] -> [(ObjCluster, ObjCluster)]
pair []     = []
pair (x:xs) = [(x,y) | y <- xs, not (areRelated x y)] ++ pair xs

-- | True if the supplied object clusters are related.
areRelated :: ObjCluster -> ObjCluster -> Bool
areRelated x y = any (== objClusterId x) (yObjs ++ ySubObjs ++ yHyponymys) &&
                 any (== objClusterId y) (xObjs ++ xSubObjs) 
                 -- Hyponymys do exist symmetrically, no need to check twice.
  where
    propKeys f c = concatMap (keys . f) (allArgClusters c) 
    xObjs = propKeys objFrequency x
    yObjs = propKeys objFrequency y
    xSubObjs = propKeys subObjFrequency x
    ySubObjs = propKeys subObjFrequency y
    yHyponymys = map objClusterId (hypernyms y ++ hyponyms y)

-- | Create a score between two object clusters.
score :: Config -> HashSet Int -> ObjCluster -> ObjCluster -> Score
score cfg unqlfSet x y = if useHyponymys cfg && not partsSimilar then electedScore else mergeScore
  where
    -- True if part similarity is above threshold.
    partsSimilar = scoreParts x y >= tsyn cfg
    allArgScores = scores x y -- All argument scores.
    electedScore = hyponymize cfg unqlfSet mergeScore {argScores = allArgScores}
    -- Merge points, merge argument scores and merge score.
    mergeScores = filterMergers cfg allArgScores
    ((_,mergePoints),_) = toScoreSet (frequencySum x) (frequencySum y) (Merge, mergeScores)
    mergeScore = Score { objLeft = x, objRight = y, scoreOp = Merge
                       , scoreValue = mergePoints, argScores = mergeScores }