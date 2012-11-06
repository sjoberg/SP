{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Module for redirecting cluster references to new ones.
module SP.Execution.Redirect where

import Control.Arrow (first, second)
import Data.HashMap.Lazy (fromList, elems, lookupDefault, toList, unionWith)
import Data.List (nub)
import SP.Cluster
import SP.Execution.Update

-- | Redirect function for updating object clusters.
redirect :: [ObjCluster] -> Update -> [ObjCluster]
redirect objClusters update = nub (newObjClusters update ++ elems deepObjMap)
  where
    -- Object cluster map.
    objMap = fromList (objTuples update ++ objTupleMap (newObjClusters update))
    oldObjMap = fromList (objTupleMap objClusters)
    deepObjMap = unionWith (const . objUpdate) objMap oldObjMap
    objTupleMap = map $ \c -> (c, objUpdate c)
    
    -- Argument cluster map.
    argMap = fromList (newArgs ++ updatedArgs)
    newArgs = map (\c -> (c, argUpdate c)) (concatMap allArgClusters $ elems objMap)
    updatedArgs = map (second argUpdate) (argTuples update)
    
    -- Update function for object cluster tuples.
    objUpdate c = c
        { parents = mapLookupArgs parents
        , children = mapLookupArgs children
        , siblings = mapLookupArgs siblings
        , hypernyms = mapLookupObjs hypernyms
        , hyponyms = mapLookupObjs hyponyms
        }
      where
        mapLookupArgs f = map (\x -> (lookupDefault x x argMap) {frequency = frequency x}) (f c)
        mapLookupObjs f = map (\x -> lookupDefault x x objMap) (f c)
    
    -- Update function for argument cluster tuples
    argUpdate c = c
        { objFrequency = mapTransform objFrequency
        , subObjFrequency = mapTransform subObjFrequency
        , isaParents = mapLookupArgs isaParents
        , isaChildren = mapLookupArgs isaChildren
        }
      where
        mapLookupArgs f = map (\x -> (lookupDefault x x argMap) {frequency = frequency x}) (f c)
        mapTransform m = fromList . map (first objLookup) . toList $ m c
        objLookup clusterId = objClusterId (lookupDefault fakeKey fakeKey objMap)
          where
            fakeKey = ObjCluster {objClusterId = clusterId}
