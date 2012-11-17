-- | Reduce graph.
module SP.Reduction.Reducer where

import Control.Arrow ((***), second)
import Data.HashMap.Lazy (HashMap, empty, fromList, foldlWithKey', insert, insertWith, lookupDefault)
import Data.List ((\\), nub)
import SP.Cluster
import SP.Reduction.Merger (merge)
import SP.Reduction.Abstraction (abstract)
import SP.Reduction.Isa (child, parent)
import SP.Reduction.Update
import SP.Scoring.Score
import Prelude hiding (lookup)

type IdMap = HashMap Int Int

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
updateDomain updateData = map update . (++ new) . (\\ old)
  where
    update = updateObj objIdMap argIdMap argMap
    (old,new) = second nub . unzip $ objTuples updateData
    -- Create argument cluster map.
    argMap = fromList (argTuples updateData)
    -- Create objId to objId map.
    objIdMap = fromList . map (objClusterId *** objClusterId) $ objTuples updateData
    -- Create argId to argId map.
    addConvert m k v = insert (argClusterId k) (argClusterId v) m
    argIdMap = foldlWithKey' addConvert empty argMap

-- | Update an object cluster.
updateObj :: IdMap -> IdMap -> HashMap ArgCluster ArgCluster -> ObjCluster -> ObjCluster
updateObj objIdMap argIdMap argMap cluster = cluster
    { parents = fixArgs parents
    , children = fixArgs children
    , siblings = fixArgs siblings
    , hypernyms = fixObjIds hypernyms
    , hyponyms = fixObjIds hyponyms
    }
  where
    fixArgs = map (\a -> updateArg objIdMap argIdMap $ lookupDefault a a argMap) . ($ cluster)
    fixObjIds = nub . map (\i -> lookupDefault i i objIdMap) . ($ cluster)

-- | Update an argument cluster.
updateArg :: IdMap -> IdMap -> ArgCluster -> ArgCluster
updateArg objIdMap argIdMap cluster = cluster
    { objFrequency = fixObjs objFrequency
    , subObjFrequency = fixObjs subObjFrequency
    , isaParents = fixArgIds isaParents
    , isaChildren = fixArgIds isaChildren
    }
  where
    -- If there is no replacement in the idMap, insert the original post.
    -- Otherwise, insert the replacement key. If there is one replacement,
    -- there will be two in total. Use insertWith (+) to add the values
    -- together, when the second has been stepped upon. Effectively, this
    -- doubles the frequencies, for the replacement post. Too concise. 
    double m k v = let k' = lookupDefault k k objIdMap in insertWith (+) k' v m 
    fixObjs = foldlWithKey' double empty . ($ cluster)
    -- Fix argument cluster ids.
    fixArgIds = map (\i -> lookupDefault i i argIdMap) . ($ cluster) 
    