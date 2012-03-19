module SP.Preprocess.Preprocess where

import Control.Parallel.Strategies as P
import Data.Function
import Data.HashMap.Lazy hiding (filter, map, null)
import Data.Maybe 
import Data.Ord
import SP.ByteString (pack, ByteString, seize)
import SP.Config
import SP.Cluster
import SP.Preprocess.Compound
import SP.Redirect

import Data.List.Stream
import Prelude hiding (head, concatMap, take, map, filter, null)

takePartitions :: Config -> [Partition] -> [Partition]
takePartitions cfg = take $ artSize cfg

groupByPos :: [Partition] -> [[ObjectCluster]]
groupByPos ptns = 
  let os = concatMap ocs ptns
      opos = seize 2 . pos . head . parts
  in groupBy ((==) `on` opos) (sortBy (comparing opos) os)

groupByForm :: [Partition] -> [[ObjectCluster]]
groupByForm ptns = 
  let os = concatMap ocs ptns
      oform = form . head . parts
  in groupBy ((==) `on` oform) (sortBy (comparing oform) os)

mergeCompounds :: [Partition] -> [Partition]
mergeCompounds = P.parMap rseq (mkNnCompounds . mkNerCompounds)

filterEmpty :: [Partition] -> [Partition]
filterEmpty ps = map rebuild ps
  where rebuild p = p {ocs = filterObjClrs $ ocs p}
        filterObjClrs = filter $ \o -> not $ null (pars o) || null (chdn o)


