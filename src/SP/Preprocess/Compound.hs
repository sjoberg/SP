module SP.Preprocess.Compound --(mkCompounds,mkNnGrps,mkNerCompounds,mkNerGrps) 
  where

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as IntMap
import Data.Function
import Data.List (groupBy, intersect, intersperse, nubBy, sortBy)
import Data.Maybe 
import Data.Ord
import Prelude hiding (concat)
import SP.ByteString (pack, concat)
import SP.Cluster
import SP.Redirect

mkCompounds :: Partition -> Partition
mkCompounds = mkNnCompounds.mkNerCompounds

mkNnCompounds :: Partition -> Partition
mkNnCompounds ptn | null grps = ptn
                  | otherwise = nptn
  where nptn = redirect ptn $ mkRedirectMap grps
        grps = mkNnGrps $ ocs ptn

mkNerCompounds :: Partition -> Partition
mkNerCompounds ptn | null grps = ptn -- TODO Improve grouping=>No recursion
                   | otherwise = if null (mkNerGrps $ ocs nptn) then nptn 
                                 else error "Ner fail!!!"
  where nptn = redirect ptn $ mkRedirectMap grps
        grps = mkNerGrps $ ocs ptn

mkRedirectMap :: [[ObjectCluster]] -> HashMap.HashMap ObjectCluster ObjectCluster
mkRedirectMap grps = HashMap.fromList $ concatMap mkRedirectList grps
  where mkRedirectList grp = map (\o -> (o, merge grp)) grp

-- Merge a group of object clusters. 
merge :: [ObjectCluster] -> ObjectCluster
merge grp = ObjectCluster ((ocId.head) grp) [part] parIs chdIs sblIs
  where -- Create part and form new token annotations.
        part = ((p.last) grp) {form = nform, lemma = nlemma, text = ntext}
        nform = concat [nlemma, pack ":", pos part]
        nlemma = concat.intersperse (pack "_") $ map (lemma.p) grp
        ntext = concat.intersperse (pack "_") $ map (text.p) grp
        p = head.parts

        -- Remove argument clusters pointing to the same object cluster
        -- in the group. Remove multiple argument clusters.
        allSbls :: IncidenceList
        allSbls = concatMap sbls grp

        -- Incidence lists for siblings, parents and children.
        sblIs = nubBy cmp2 oacs
          where oacs :: IncidenceList
                oacs = filter f allSbls 
                f :: (ArgumentCluster, Incidence) -> Bool
                f (a,_) = null $ [(intKey.parMap.acFst) a, 
                                  (intKey.chdMap.acSnd) a] 
                                  `intersect` 
                                  map ocId grp
                cmp2 (x,_) (y,_) = cmp parMap (acFst x) (acFst y) && 
                                   cmp chdMap (acSnd x) (acSnd y)
        parIs = map (\a -> (a,1)) $ nubBy (cmp parMap) $ acs pars parMap
        chdIs = map (\a -> (a,1)) $ nubBy (cmp chdMap) $ acs chdn chdMap
        -- Get all argument clusters via method acc, that aren't pointing
        -- within the group via object map m.
        acs acc m = [a | i <- concatMap acc grp, let a = fst i, inGrp a]
          where inGrp a = (intKey.m) a `notElem` map ocId grp
        -- Compare two incidence tuples with respect to where their 
        -- object clusters are pointing via object map m, and their 
        -- relation.
        cmp m ai aj = (intKey.m) ai == (intKey.m) aj && 
                      (key.relMap) ai == (key.relMap) aj
        intKey = head . IntMap.keys; key = head . HashMap.keys

{-mkNerGrps :: [ObjectCluster] -> [[ObjectCluster]]
mkNerGrps ocs = filter (\g -> (cner.head) g /= pack "O" && length g > 1) groups
  where groups = groupBy pred $ sortBy (comparing ocId) ocs
          where pred oi oj = let conn | cner oi == pack "O" = True                       
                                      | otherwise = ocId oj `elem` neighbors oi
                             in (cner oi == cner oj) && conn
        neighbors oi = parObjIds oi ++ chdObjIds oi -- ++ sblObjIds oi
        cner = ner.head.parts -- Ner of the first (only) part in a cluster.
-}

mkNerGrps ocs = filter (\g -> (cner.head) g /= pack "O" && length g > 1) groups
  where groups = groupBy pred $ sortBy (comparing ocId) ocs
        pred oi oj = let eqNer = (==) `on` cner
                         eqSnt = (==) `on` sntId.head.parts
                         eqArt = (==) `on` artId.head.parts
                     in eqNer oi oj && eqSnt oi oj && eqArt oi oj
        cner = ner.head.parts

{-
mkNnGrps :: [ObjectCluster] -> [[ObjectCluster]]
mkNnGrps ocs = groupBy pred $ sortBy (comparing ocId) $ filter oHasRelNn ocs
  where pred oi oj = any (conn chdMap) (acs chdn oi) || 
                     any (conn parMap) (acs pars oi)
          where conn m a = aHasRelNn a && aHasO a oj m
        oHasRelNn o = any aHasRelNn (acs chdn o) || 
                      any aHasRelNn (acs pars o)
        aHasRelNn a = isJust $ HashMap.lookup (pack "nn") (relMap a)
        aHasO a o m = isJust $ IntMap.lookup (ocId o) (m a)
        acs l o = map fst $ l o
-}

mkNnGrps :: [ObjectCluster] -> [[ObjectCluster]]
mkNnGrps ocs = map (sortBy (comparing ocId).getWithChdn).filter oHasRelNn $ ocs
  where oHasRelNn o = any aHasRelNn (map fst $ chdn o)
        aHasRelNn a = isJust $ HashMap.lookup (pack "nn") (relMap a)
        aHasO a o m = isJust $ IntMap.lookup (ocId o) (m a)
        idOcMap = IntMap.fromList $ map (\o -> (ocId o,o)) ocs
        getWithChdn x = x:(concatMap getWithChdn $ chdObjs x)
        chdObjs = idsToObjs.concatMap (IntMap.keys.chdMap).filter aHasRelNn.map fst.chdn
        idsToObjs = map $ \id -> fj "NnGrps" $ IntMap.lookup id idOcMap

fj s m | m == Nothing = error s
       | otherwise = fromJust m
