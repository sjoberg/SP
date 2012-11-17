-- | Update instructions for parent and child operators.
module SP.Reduction.Isa where

import SP.Cluster
import SP.Reduction.Merger (merge)
import SP.Reduction.Update
import SP.Reduction.Turnable
import SP.Scoring.Score
import Data.HashMap.Lazy (HashMap, fromList, lookupDefault)

-- | Update for the parent operator.
parent :: Score -> Update
parent = child . turn

-- | Update for the child operator.
child :: Score -> Update
child score@Score {objLeft = x, objRight = y, argScores = scores} = emptyUpdate
    { objTuples = [(x,x'),(y,y')]
    , argTuples = argTuples'
    }
  where
    -- Child.
    x' = x {hypernyms = objClusterId y' : hypernyms x}
    -- Parent.
    yUpdate = merge (turn score)
    z = snd . head . objTuples $ yUpdate
    y' = z {hyponyms = objClusterId x' : hyponyms y}
    argTuples' = concatMap (scoreToTuples $ fromList . argTuples $ yUpdate) scores
    -- All the unused argument clusters in the child object cluster will
    -- function as merged clusters. No change is needed to them. Each time
    -- they, in the parent or child respectively, are updated in an execution,
    -- they will be updated also in the child or parent respectively. There's
    -- no need for any ISA relation to be marked out for the unused clusters.

-- | Convert a score to tuples. The tuples made is to replace argument
-- clusters with copies that include ISA links (for child scores).
scoreToTuples :: HashMap ArgCluster ArgCluster -> ArgScore -> [(ArgCluster,ArgCluster)]
scoreToTuples argMap ArgScore {argLeft = x, argRight = y, argScoreOp = op} = case op of
    Merge -> [(x,z {frequency = frequency x}),(y,z)]
    Child -> let x' = x {isaParents = argClusterId y' : isaParents x} -- Has correct frequency.
                 y' = z {isaChildren = argClusterId x' : isaChildren z}
             in [(x,x'),(y,y')]
    _ -> error "Invalid operator type during ISA execution."
  where
    -- Merged cluster. Lookup from y since the ids has been flipped.
    z = lookupDefault (error "ISA execution error") y argMap
