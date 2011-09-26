module SP.Merge (mergeClusters) where

import Data.HashMap.Lazy (fromList, HashMap, lookupDefault)
import Data.List
import SP.Cluster
import SP.Score

type ObjClrMap = HashMap ObjClr ObjClr

-- | Merge all clusters in accordance with a list of good scores.
mergeClusters :: [ObjClr] -> [OpScr] -> [ObjClr]
mergeClusters objClrs scores = redir ++ new
  where
    -- Object cluster map, mapping unused and used to redirected and new.
    ocm = fromList $ newTpls ++ redirTpls
    -- Redirected unused object clusters.
    redir = snd $ unzip redirTpls 
    -- New object clusters.
    new = nub $ snd $ unzip newTpls
    -- Tuples used for merge.
    used = concatMap (\s -> map ($ scr s) [sOc1, sOc2]) scores
    -- Unused object clusters.
    unused = filter (`notElem` used) objClrs
    -- New tuples (redirected).
    newTpls = concat $ zipWith (mergeObjClrs ocm) newIds scores
      where newIds = [ocId (snd $ last redirTpls) + 1..]
    -- Tuples of unused and their redirected copies.
    redirTpls = zipWith (mkObjClr ocm) [ocId (last objClrs) + 1..] unused

-- | Create a new argument cluster. 
-- | Returns a list of pairs containing a used cluster as the first element, 
-- | and the new cluster as the second
mergeObjClrs :: ObjClrMap -> Int -> OpScr -> [(ObjClr,ObjClr)]
mergeObjClrs ocm id s = [((oc1 . objScr . scr) s, oc), 
                         ((oc2 . objScr . scr) s, oc)]
  where 
    oc = ObjClr id parts (acsByRole Par) (acsByRole Chd) (acsByRole Sbl)
    parts = ocParts (oc1 os) ++ ocParts (oc2 os) where os = objScr $ scr s
    argClrs = mergeAllArgClrs ocm oc $ scr s
    -- Get argument clusters by role.
    acsByRole r = filter (\ac -> acRole ac == r) argClrs

-- | Make new argument clusters by merging existing ones or copying.
mergeAllArgClrs :: ObjClrMap -> ObjClr -> Scr -> [ArgClr]
mergeAllArgClrs ocm oc (Scr os ass u1 u2) = clustered ++ unclustered
  where
    -- Results of the cluster merges.
    clustered = zipWith (\s -> mergeArgClrs ocm oc (ac1 s) (ac2 s)) ass [1..]
    -- Copies of the scores that were not merged. orginals can't be used,
    -- since they point to the wrong object cluster and have the wrong id.
    unclustered = zipWith (mkArgClr ocm oc) orgUnclustered [length ass + 1..]
    -- orginal argument clusters not merged.
    orgUnclustered = u1 ++ u2 -- filter (`notElem` usedArgClrs) orgArgClrs
    -- The orginal argument clusters of both object clusters.
    orgArgClrs = argClrs (oc1 os) ++ argClrs (oc2 os)
      where argClrs oc = parArgClrs oc ++ chdArgClrs oc ++  sblArgClrs oc
    -- orginal argument clusters used for merging.
    usedArgClrs = concat [[ac1 as, ac2 as] | as <- ass]

-- | Create a new argument cluster by merging two existing ones.
mergeArgClrs :: ObjClrMap -> ObjClr -> ArgClr -> ArgClr -> Int -> ArgClr
mergeArgClrs ocm oc (AdjArgClr _ _ args1 role) (AdjArgClr _ _ args2 _) id = 
  AdjArgClr id oc (map (rplAdjArg ocm) $ args1 ++ args2) role
mergeArgClrs ocm oc (RmtArgClr _ _ args1 role) (RmtArgClr _ _ args2 _) id = 
  RmtArgClr id oc (map (rplRmtArg ocm) $ args1 ++ args2) role

-- | Create an argument cluster from an existing one.
mkArgClr :: ObjClrMap -> ObjClr -> ArgClr -> Int -> ArgClr
mkArgClr ocm oc ac@AdjArgClr {aacArgs=args} id = 
  ac {aacId=id, aacObj=oc, aacArgs=map (rplAdjArg ocm) args}
mkArgClr ocm oc ac@RmtArgClr {racArgs=args} id = 
  ac {racId=id, racObj=oc, racArgs=map (rplRmtArg ocm) args}

-- | Make a new object cluster with updated references.
mkObjClr :: ObjClrMap -> Int -> ObjClr -> (ObjClr, ObjClr)
mkObjClr ocm id oc@ObjClr {parArgClrs=oldPacs, chdArgClrs=oldCacs, 
                           sblArgClrs=oldSacs, ocParts=oldParts} = (oc, ocNew)
  where 
    ocNew = ObjClr id oldParts pacs cacs sacs
    pacs = mapArgClrs oldPacs -- Parent argument clusters.
    cacs = mapArgClrs oldCacs -- Child argument clusters.
    sacs = mapArgClrs oldSacs -- Sibling argument clusters.
    mapArgClrs = map (\ac -> mkArgClr ocm ocNew ac $ acId ac)

-- | Eventually substitutes an object cluster.
rplObjClr :: ObjClrMap -> ObjClr -> ObjClr
rplObjClr ocm oc = lookupDefault oc oc ocm
rplAdjArg :: ObjClrMap -> AdjArg -> AdjArg
rplAdjArg ocm arg@AdjArg {obj = oc} = arg {obj = rplObjClr ocm oc}
rplRmtArg :: ObjClrMap -> RmtArg -> RmtArg
rplRmtArg ocm arg@RmtArg {intObj = intObj, rmtObj = rmtObj} = 
  arg {intObj = rplObjClr ocm intObj, rmtObj = rplObjClr ocm rmtObj}

