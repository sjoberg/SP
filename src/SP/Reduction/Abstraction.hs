-- | Execution of abstract operations. TODO Check math.
module SP.Reduction.Abstraction where

import SP.Cluster
import SP.Reduction.Merger (mergeArg, ns, updateMap)
import SP.Reduction.Update
import SP.Scoring.Score
import Data.List ((\\), intersect, nub)

abstract :: Score -> Update
abstract Score {objLeft = x, objRight = y, argScores = scores} = Update
    { newObjClusters = [z]
    , objTuples = [(x,x'),(y,y')]
    , argTuples = argTuples'
    }
  where
    argTuples' = concatMap (procArg (ns x) (ns y)) scores
    commonHypernyms = hypernyms x `intersect` hypernyms y
    x' = x {hypernyms = hypernyms x \\ commonHypernyms}
    y' = y {hypernyms = hypernyms y \\ commonHypernyms}
    z = x { objClusterId = negate (objClusterId x)
          , numSamples = numSamples x + numSamples y
          , parts = parts x ++ parts y
          , hypernyms = commonHypernyms
          , hyponyms = [x,y]
          , parents = unused parents ++ new parents
          , children = unused children ++ new children
          , siblings = unused siblings ++ new siblings
          }
    -- New argument clusters.
    new f = filter (`elem` f x) . nub $ map snd argTuples'
    -- Unused argument clusters.
    unused f = (f x \\ map argLeft scores) ++ (f y \\ map argRight scores)

-- | Merge or abstract argument cluster(s).
procArg :: Double -> Double -> ArgScore -> [(ArgCluster,ArgCluster)]
procArg nsx nsy score = case argScoreOp score of
    Merge -> mergeArg nsx nsy score
    Abstract -> abstractArg nsx nsy score
    _ -> error "Invalid argument score operator for abstract operation." 

-- | Abstract an argument cluster.
abstractArg :: Double -> Double -> ArgScore -> [(ArgCluster,ArgCluster)]
abstractArg nsx nsy ArgScore {argLeft = x, argRight = y} = [(x,x'),(y,y'),(z,z)]
  where
    commonParents = isaParents x `intersect` isaParents y
    x' = x {isaParents = isaParents x \\ commonParents}
    y' = y {isaParents = isaParents y \\ commonParents}
    z = ArgCluster
        { argClusterId = negate (argClusterId x)
        , frequency = (frequency x * nsx + frequency y * nsy) / (nsx + nsy)
        , numArgs = numArgs x + numArgs y
        , isaParents = commonParents
        , isaChildren = [x,y]
        , objFrequency = updateMap x y objFrequency
        , relFrequency = updateMap x y relFrequency
        , subObjFrequency = updateMap x y subObjFrequency
        , subRelFrequency = updateMap x y subRelFrequency
        }
