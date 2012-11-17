-- | Reduce graph. TODO ISA support.
module SP.Reduction.Reducer where

import Control.Arrow ((***), second)
import Data.HashMap.Lazy (HashMap, empty, fromList, foldlWithKey', insertWith, lookupDefault)
import Data.List ((\\), nub)
import SP.Cluster
import SP.Reduction.Merger (merge)
import SP.Reduction.Abstraction (abstract)
import SP.Reduction.Isa (child, parent)
import SP.Reduction.Update
import SP.Scoring.Score
import Prelude hiding (lookup)

-- | Execute a set of scores on a partition of object clusters.
reduce :: [ObjCluster] -> [Score] -> [ObjCluster]
reduce clusters = flip updateDomain clusters . foldUpdates . map scoreToUpdate

-- | Create an update instruction given a operator score.
scoreToUpdate :: Score -> Update
scoreToUpdate score = case scoreOp score of
    Merge -> merge score
    Abstract -> abstract score
    Child -> child score
    Parent -> parent score

-- | Update the domain, given old domain and update data.
updateDomain :: Update -> [ObjCluster] -> [ObjCluster]
updateDomain updateData = map (updateObj idMap argMap) . (++ new) . (\\ old)
  where
    (old,new) = second nub . unzip $ objTuples updateData
    argMap = fromList (argTuples updateData)
    idMap = fromList . map (objClusterId *** objClusterId) $ objTuples updateData

-- | Update an object cluster.
updateObj :: HashMap Int Int -> HashMap ArgCluster ArgCluster -> ObjCluster -> ObjCluster
updateObj idMap argMap cluster = cluster
    { parents = fix parents
    , children = fix children
    , siblings = fix siblings
    }
  where
    fix f = map (\a -> updateArg idMap $ lookupDefault a a argMap) (f cluster) 

-- | Update an argument cluster.
updateArg :: HashMap Int Int -> ArgCluster -> ArgCluster
updateArg idMap cluster = cluster
    { objFrequency = fix objFrequency
    , subObjFrequency = fix subObjFrequency
    }
  where
    -- If there is no replacement in the idMap, insert the original post.
    -- Otherwise, insert the replacement key. If there is one replacement,
    -- there will be two in total. Use insertWith (+) to add the values
    -- together, when the second has been stepped upon. Effectively, this
    -- doubles the frequencies, for the replacement post. Too concise. 
    double m k v = let k' = lookupDefault k k idMap in insertWith (+) k' v m 
    fix f = foldlWithKey' double empty (f cluster)
