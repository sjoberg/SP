{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module SP.Redirect where

import Control.Arrow (first)
import Control.DeepSeq
import Data.List.Stream
import Data.Maybe
import Data.HashMap.Lazy as HashMap hiding (filter, map)
import qualified Data.IntMap as IntMap
import SP.Cluster
import SP.DeepSeq
import Prelude hiding (filter,map,notElem,unzip,(++))

-- | Redirects object clusters and argument clusters in a partion
-- given a map from object clusters to delete, to new ones.
redirect :: Partition -> HashMap ObjectCluster ObjectCluster -> Partition
redirect ptn objMap = newPtn `deepseq` newPtn 
  where
  newPtn = ptn {ocs = nub $ elems objMapDeep, acs = map aUpdate $ acs ptn}
--new, del, keep :: [ObjectCluster]
--delNew = unzip $ toList objMap
--del = fst delNew -- Object clusters to remove.
--new = nub $ snd delNew -- New object clusters.
--keep = filter (`notElem` del) (ocs ptn) -- Old object clusters to keep.
  
  -- Maps for old to new object and argument clusters.
  objMapDeep :: HashMap ObjectCluster ObjectCluster
  objMapDeep = unionWith merge objMap (fromList $ map toTpl $ ocs ptn) --keep ++ new ++ del)
    where toTpl o = (o, oUpdate o); merge o _ = oUpdate o
  argMap = fromList $ map (\a -> (a, aUpdate a)) (acs ptn)
  
  -- Redirect an object cluster.
  oUpdate :: ObjectCluster -> ObjectCluster
  oUpdate o@(ObjectCluster {pars = pars, chdn = chdn, sbls = sbls}) = 
    o {pars = itUpdate pars, chdn = itUpdate chdn, sbls = itUpdate sbls}
    where itUpdate = map get -- = map first aUpdate
          get it = (lookup (fst it) argMap, snd it)
  
  lookup k = fromJust.HashMap.lookup k

  -- Redirect an argument cluster.
  aUpdate :: ArgumentCluster -> ArgumentCluster
  aUpdate a@(ArgumentCluster {parMap = parMap, chdMap = chdMap}) = 
    a {parMap = amUpd parMap, chdMap = amUpd chdMap}
    where amUpd m = IntMap.fromList $ map itUpdate (IntMap.toList m)
          itUpdate (id,inc) = (ocId $ lookup oKey objMapDeep, inc)
            where oKey = ObjectCluster {ocId = id}
  aUpdate (D2ArgumentCluster x y) = D2ArgumentCluster (aUpdate x) (aUpdate y)
  
