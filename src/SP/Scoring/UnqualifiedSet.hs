-- | Module for looking up sample counts.
module SP.Scoring.UnqualifiedSet where

import Data.HashMap.Lazy (HashMap, elems, filterWithKey, foldl', intersectionWith)
import Data.HashSet (HashSet, fromList, member)
import SP.Cluster

-- | Create a set of object cluster ids of object clusters with samples under threshold.
sampleSet :: Int -> [ObjCluster] -> HashSet Int
sampleSet threshold = fromList . map objClusterId . filter ((< threshold) . numSamples)

filterMap :: HashSet Int -> HashMap Int Double -> HashMap Int Double
filterMap set = filterWithKey $ \k _ -> not (k `member` set)

-- | Sample truncated intersection errors. Does not use object clusters with few samples.
truncIntersectionErrs :: HashSet Int -> ArgCluster -> ArgCluster -> [Double]
truncIntersectionErrs unqlfSet x y = elems match
  where
    match = intersectionWith (\p q -> abs (p * xSize - q * ySize)) xQlfMap yQlfMap
    -- Maps with keys of only qualified object clusters.
    xQlfMap = filterMap unqlfSet (endObjMap x)
    yQlfMap = filterMap unqlfSet (endObjMap y)
    -- We need to weigh down incidences, so that their sum equals 1 for the qualified map.
    xSize = foldl' (+) 0 xQlfMap
    ySize = foldl' (+) 0 yQlfMap