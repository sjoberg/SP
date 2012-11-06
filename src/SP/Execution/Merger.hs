-- | Module for building merger updates from scores.
module SP.Execution.Merger where

import SP.Cluster
import SP.Scoring.Score
import SP.Execution.Update
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap, union, difference, unionWith)
import qualified Data.HashMap.Lazy as HashMap (map)
import Data.List ((\\), nub)

-- | Merge two object clusters.
merge :: Score -> Update
merge Score {objLeft = x, objRight = y, argScores = scores} = emptyUpdate
    { objTuples = [(x,z),(y,z)]
    , argTuples = argTuples'
    }
  where
    -- Argument tuples for redirection.
    argTuples' = concatMap (mergeArg (ns x) (ns y)) scores
    -- Merged argument cluster.
    z = x { numSamples = numSamples x + numSamples y
          , parts = parts x ++ parts y
          , hypernyms = hypernyms x ++ hypernyms y
          , hyponyms = hyponyms x ++ hyponyms y
          , parents = unused parents ++ new parents
          , children = unused children ++ new children
          , siblings = unused siblings ++ new siblings
          }
    -- New argument clusters that are parents, children, or whatever.
    new f = filter (`elem` f x) . nub $ map snd argTuples'
    -- Unclustered argument clusters that are parents, children, or whatever.
    unused f = (f x \\ map argLeft scores) ++ (f y \\ map argRight scores)

-- | Number of samples as a double.
ns :: ObjCluster -> Double
ns = fromIntegral . numSamples 

-- | Merge two argument clusters to create a third.
mergeArg :: Double -> Double -> ArgScore -> [(ArgCluster, ArgCluster)]
mergeArg nsx nsy ArgScore {argLeft = x, argRight = y} = [(x,z),(y,z)]
  where           
    z = x { frequency = (frequency x * nsx + frequency y * nsy) / (nsx + nsy)
          , numArgs = numArgs x + numArgs y
          , isaParents = isaParents x ++ isaParents y
          , objFrequency = updateMap x y objFrequency
          , subObjFrequency = updateMap x y subObjFrequency
          , relFrequency = updateMap x y relFrequency
          , subRelFrequency = updateMap x y subRelFrequency 
          }
    
-- | Merge HashMaps for property value frequencies.
updateMap :: (Eq k, Hashable k) => ArgCluster -> ArgCluster -> (ArgCluster -> HashMap k Double) 
          -> HashMap k Double
updateMap x y f = ux `union` uy `union` m
  where
    nax = fromIntegral (numArgs x) -- Number of arguments for x
    nay = fromIntegral (numArgs y) -- Number of arguments for y
    -- Map of unmerged property values from x and y.
    ux = HashMap.map (* (nax / (nax + nay))) $ difference (f x) (f y)
    uy = HashMap.map (* (nay / (nax + nay))) $ difference (f y) (f x)
    -- Merged property value map.
    m = unionWith (\w z -> (nax * w + nay * z) / (nax + nay)) (f x) (f y)
{-# INLINE updateMap #-}
