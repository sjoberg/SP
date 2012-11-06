-- | Shared functions for composition.
module SP.Composition.Common where

import Data.HashMap.Lazy (keys)
import Data.List (intersect)
import SP.Cluster

-- | Make a compound object cluster.
mkCompoundObjCluster :: Part -> [ObjCluster] -> ObjCluster
mkCompoundObjCluster compPart objClusters = ObjCluster
    { objClusterId = objClusterId $ head objClusters
    , numSamples = 1
    , parts = [compPart]
    , hypernyms = []
    , hyponyms = []
    , parents = joinArgClusters parents objClusters
    , children = joinArgClusters children objClusters
    , siblings = joinArgClusters siblings objClusters
    }

-- | Join argument clusters with the given property accessor.
joinArgClusters :: (ObjCluster -> [ArgCluster]) -> [ObjCluster] -> [ArgCluster]
joinArgClusters prop cs = filterArgClusters cs (concatMap prop cs)
{-# INLINE joinArgClusters #-}

-- | Use only argument clusters directed outside the compound.
filterArgClusters :: [ObjCluster] -> [ArgCluster] -> [ArgCluster]
filterArgClusters cs = filter $ \c -> null $ objs c `intersect` map objClusterId cs
  where
    objs c = (keys . objFrequency) c ++ (keys . subObjFrequency) c
{-# INLINE filterArgClusters #-}