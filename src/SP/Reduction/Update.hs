-- | Updates to be used for operator execution.
module SP.Reduction.Update where

import Data.HashMap.Lazy (HashMap, fromList)
import SP.Cluster

-- | Update data type.
data Update = Update
    { newObjClusters    :: [ObjCluster]                 -- ^ New object clusters.            
    , objTuples         :: [(ObjCluster, ObjCluster)]   -- ^ Object cluster tuples.
    , argTuples         :: [(ArgCluster, ArgCluster)]   -- ^ Argument cluster tuples.
    } deriving (Show)
    
-- | Empty update constructor.
emptyUpdate :: Update
emptyUpdate = Update [] [] []

-- | Update to object cluster map.
toObjClusterMap :: Update -> HashMap ObjCluster ObjCluster
toObjClusterMap = fromList . objTuples

-- | Update to argument cluster map.
toArgClusterMap :: Update -> HashMap ArgCluster ArgCluster
toArgClusterMap = fromList . argTuples

-- | Merge two updates.
mergeUpdate :: Update -> Update -> Update
mergeUpdate x y = Update
    { newObjClusters = newObjClusters x ++ newObjClusters y
    , objTuples = objTuples x ++ objTuples y
    , argTuples = argTuples x ++ argTuples y
    }

-- | Merge a list of updates.
foldUpdates :: [Update] -> Update
foldUpdates = foldl1 mergeUpdate