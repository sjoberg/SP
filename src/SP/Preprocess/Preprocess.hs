module SP.Preprocess.Preprocess where

import Control.Parallel.Strategies as P
import Data.Function
import Data.HashMap.Lazy hiding (filter, map, null)
import Data.List.Stream
import Data.Maybe 
import Data.Ord
import SP.ByteString (pack, ByteString)
import SP.Config
import SP.Cluster
import SP.Preprocess.Compound
import SP.Redirect
import Prelude hiding (head,concatMap,take,map,filter,null)

takePartitions :: Config -> [Partition] -> [Partition]
takePartitions cfg = take $ artSize cfg

groupByPos :: [Partition] -> [[ObjectCluster]]
groupByPos ptns = let os :: [ObjectCluster]
                      os = concatMap ocs ptns
                      opos :: ObjectCluster -> ByteString
                      opos = pos.head.parts
                  in groupBy ((==) `on` opos) (sortBy (comparing opos) os)

mergeNerCompounds :: [Partition] -> [Partition]
mergeNerCompounds = P.parMap rseq mkNerCompounds

filterEmpty :: [Partition] -> [Partition]
filterEmpty ps = map rebuild ps
  where rebuild p = p {ocs = filterObjClrs $ ocs p}
        filterObjClrs = filter $ \o -> not $ null (pars o) || null (chdn o)
