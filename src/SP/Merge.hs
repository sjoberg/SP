module SP.Merge where

import qualified Control.Parallel.Strategies as Parallel
import Control.Arrow
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as IntMap
import Data.List (foldl1', foldl')
import Data.Maybe
import Data.Function
import Data.Hashable
import Data.Tuple

import SP.Cluster
import SP.Redirect
import SP.Score.Score

{-
-- | Merge some, among a list of partitions, by object scores.
mergePartitions :: [ObjectScore] -> [Partition] -> [Partition]
mergePartitions scores partitions =

  let -- Map from object to partition.
      objPtn :: HashMap.HashMap ObjectCluster Partition
      objPtn = let tuples :: Partition -> [(ObjectCluster,Partition)]
                   tuples p = map (\objClr -> (objClr,p)) (ocs p)
               in HashMap.fromList . concatMap tuples $ partitions

      -- Pairs for creating a node to node map.
      pairs :: [(Partition,Partition)]
      pairs = let find a s = fromJust $ HashMap.lookup (a s) objPtn
                  unprocessed = map (find o1 &&& find o2) scores
                  filter = Stream.filter $ uncurry (/=)
                  nub = nubBy $ \(x,y) (z,w) -> (x == z && y == w)
                                             || (x == w && y == z)
              in nub . filter $ unprocessed

      -- Create a map from nodes to nodes.
      edgeMap :: HashMap.HashMap Partition [Partition]
      edgeMap = let list :: [(Partition,Partition)]
                    list = pairs ++ map swap pairs
                    toList :: Partition -> [Partition]
                    toList = replicate 1
                in fromListWith (++) . map (second toList) $ list
      
      -- Create groups and merge them, to create a list of partitions.
      createGroups :: [(Partition,Partition)] -> [[Partition]] -> [Partition]
      createGroups []            groups = map merge groups
      createGroups ((p,q):pairs) groups = 
        if continue then createGroups pairs groups
                    else createGroups pairs $ group : groups
        where continue :: Bool
              continue = null . intersect [p,q] $ concat groups
              group :: [Partition]
              group = nub $ ((++) `on` build [p,q]) p q
              linkedTo :: Partition -> [Partition]
              linkedTo k = fromJust $ HashMap.lookup k edgeMap
              build :: [Partition] -> Partition -> [Partition]
              build group partition | null new  = union
                                    | otherwise = concatMap (build union) new
                where new = group \\ linkedTo partition
                      union = group ++ new

     
      -- Object map to be used for redirection.
      objMap :: HashMap.HashMap ObjectCluster ObjectCluster
      objMap = let append ts s = let new = mergeObjClrs s
                                 in (o1 s,new):(o2 s,new):ts 
               in HashMap.fromList . foldl' append [] $ scores
      
      -- All undirected partitions.
      partitions = createGroups pairs []

  in Parallel.parMap Parallel.rseq (`redirect` objMap) partitions
-}

mergePartitionsSimple :: [OperatorScore] -> [Partition] -> [Partition]
mergePartitionsSimple scores partitions = 
  
  let -- Object map to be used for redirection.
      objMap :: HashMap.HashMap ObjectCluster ObjectCluster
      objMap = let append ts s = let new = mergeObjClrs (objScr s) (argScrs s)
                                 in (o1 $ objScr s,new):(o2 $ objScr s,new):ts 
               in HashMap.fromList . foldl' append [] $ scores

      partition = merge partitions

  in [redirect partition objMap]

-- Merge two partitions.
merge :: [Partition] -> Partition
merge = foldl1' (\p n -> p {ocs = ocs p ++ ocs n, acs = acs p ++ acs n})
 
-- | From list. Merge duplicate keys using function f.
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k,v)] 
             -> HashMap.HashMap k v
fromListWith f = foldl' (\m (k,v) -> HashMap.insertWith f k v m) HashMap.empty

-- | Merge two object clusters.
mergeObjClrs :: ObjectScore -> [ArgumentScore] -> ObjectCluster
mergeObjClrs ObjectScore {o1 = oi, o2 = oj} argScrs = 

  let -- Build incidence tuples for argument clusters.
      buildIncidences proband = 
        let ui = bisect oi $ filter (\ik -> fst ik `notElem` mi) (proband oi)
            uj = bisect oj $ filter (\il -> fst il `notElem` mj) (proband oj)
            m = map (mergeArgClrs oi oj) . filter inProband $ argScrs
            inProband s = a1 s `elem` map fst (proband oi)
        in ui ++ uj ++ m

      -- New argument clusters.
      newParents  = buildIncidences pars
      newChildren = buildIncidences chdn
      newSiblings = buildIncidences sbls

      -- New parts.
      newParts = parts oi ++ parts oj

      -- Bisect incidences.
      bisect :: ObjectCluster -> [(ArgumentCluster,Incidence)] 
             -> [(ArgumentCluster,Incidence)]
      bisect oi = map $ second ( * (samples oi / (samples oi + samples oj)))
      
      -- Merged argument clusters.
      m = map mergeArgClrs

      -- Argument clusters to merge from oi and oj respectively.
      (mi,mj) = foldl' (\(x,y) n -> (a1 n:x, a2 n:y)) ([],[]) argScrs

  in oi { parts = newParts
        , pars  = newParents
        , chdn  = newChildren
        , sbls  = newSiblings }

-- | Merge two argument clusters.
mergeArgClrs :: ObjectCluster -> ObjectCluster -> ArgumentScore 
             -> (ArgumentCluster,Incidence)
mergeArgClrs oi oj ArgumentScore {a1 = ai, a2 = aj, i1 = ii, i2 = ij} = 

  let merge ai aj =
        let update macc map diff union unionWith = 
              let mi = macc ai;    mj = macc aj
                  si, sj :: Double
                  si = numArgs ai; sj = numArgs aj
                  u1 = map (* (si / (si + sj))) $ diff mi mj
                  u2 = map (* (sj / (si + sj))) $ diff mj mi
                  m = let fm x y = (si * x + sj * y) / (si + sj)
                      in unionWith fm mi mj
              in u1 `union` u2 `union` m

            parentMap   = update parMap IntMap.map    IntMap.difference 
                                        IntMap.union  IntMap.unionWith
            childMap    = update chdMap IntMap.map    IntMap.difference
                                        IntMap.union  IntMap.unionWith
            relationMap = update relMap HashMap.map   HashMap.difference
                                        HashMap.union HashMap.unionWith
        in ai {parMap = parentMap, chdMap = childMap, relMap = relationMap}
      incidence = (ii * samples oi + ij * samples oj) / 2

  in case (ai,aj) of
    (ArgumentCluster {}, ArgumentCluster {}) -> (merge ai aj, incidence)

    (D2ArgumentCluster ah ak, D2ArgumentCluster al am) ->
      (D2ArgumentCluster {acFst = merge ah al, acSnd = merge ak am}, incidence)
    (_, _) -> error "Invalid argument score."

