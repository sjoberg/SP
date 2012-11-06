-- | Module with utility functions for preprocessing.
module SP.Boot.Preprocess where

import Data.ByteString.Char8 (ByteString, take, pack)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import SP.Cluster
import SP.Config

-- | Remove object clusters without arguments.
removeEmpty :: [ObjCluster] -> [ObjCluster]
removeEmpty = filter $ \c -> 
    not (null (parents c) && null (children c) && null (siblings c)) 

-- | Remove object clusters with unallowed POS tags, set in the configuration file.
removeByPos :: [ObjCluster] -> IO [ObjCluster]
removeByPos objClusters = do
    config <- getConfig
    return $ filter ((`notElem` map pack (ignPosTags config)) . pos . head . parts) objClusters

-- | Truncate data set, using minimum allowed sample size in the configuration file.
truncate :: [ObjCluster] -> IO [ObjCluster]
truncate objClusters = do
    config <- getConfig
    return $ filter ((>= minSmpSize config) . numSamples) objClusters

-- | POS tag category. E.g. doesn't distinguish between different types of verbs,
-- neither between different types of nouns.
posCat :: ObjCluster -> ByteString
posCat = Data.ByteString.Char8.take 2 . pos . head . parts

-- | Group by POS tag category.
posCatGroups :: [ObjCluster] -> [[ObjCluster]]
posCatGroups = groupBy ((==) `on` posCat) . sortBy (comparing posCat)

-- | Lemma groups within the same pos category.
lemmaGroups :: [ObjCluster] -> [[ObjCluster]]
lemmaGroups = let f = groupBy ((==) `on` (lemma . head . parts))
                  g = sortBy . comparing $ lemma . head . parts
              in concatMap (f . g) . posCatGroups
