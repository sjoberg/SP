module SP.Merge (mergeClusters) where

import SP.Cluster
import SP.Score

-- * Merge all clusters in accordance with a list of good scores.
mergeClusters :: [ObjClr] -> [Score] -> ([ObjClr], [ObjClr], [ObjClr])
mergeClusters objClrs scores = (untouched, newClusters, usedClusters)
  where
    newClusters = [mkObjClr id s | (id, s) <- zip [ocId (last objClrs) + 1..] scores]
    untouched = deleteAll usedClusters objClrs
    usedClusters = concat [[oc1 $ objScore s, oc2 $ objScore s] | s <- scores]

-- * Make a new argument cluster.
mkObjClr :: Int -> Score -> ObjClr
mkObjClr id s = oc
  where 
    oc = ObjClr id (joinParts s) (acsByRole aacRole Par) (acsByRole aacRole Chd) (acsByRole racRole Sbl)
    joinParts (Score _ os _) = ocParts (oc1 os) ++ ocParts (oc2 os)
    argClrs = mkArgClrs oc s
    acsByRole f r = filter (\ac -> f ac == r) argClrs

-- * Make new argument clusters by merging existing ones or copying.
mkArgClrs :: ObjClr -> Score -> [ArgClr]
mkArgClrs oc (Score _ os ass) = clustered ++ unclustered
  where
    -- Results of the cluster merges.
    clustered = [mergeArgClrs id oc (ac1 as) (ac2 as) | (id, as) <- zip [1..] ass] 
    -- Copies of the scores that were not merged. Originals can't be used,
    -- since they point to the wrong object cluster and have the wrong id.
    unclustered = [mkArgClr id oc orig | (id, orig) <- zip [length ass + 1..] origUnclustered]
    -- Original argument clusters not merged.
    origUnclustered = deleteAll usedArgClusters origArgClusters
    -- The original argument clusters of both object clusters.
    origArgClusters = argClrs (oc1 os) ++ argClrs (oc2 os)
      where argClrs oc = parArgClrs oc ++ chdArgClrs oc ++  sblArgClrs oc
    -- Original argument clusters used for merging.
    usedArgClusters = concat [[ac1 as, ac2 as] | as <- ass]

-- * Create a new argument cluster by merging two existing ones.
mergeArgClrs :: Int -> ObjClr -> ArgClr -> ArgClr -> ArgClr
mergeArgClrs id oc (AdjArgClr _ _ args1 role) (AdjArgClr _ _ args2 _) = AdjArgClr id oc (args1 ++ args2) role
mergeArgClrs id oc (RmtArgClr _ _ args1 role) (RmtArgClr _ _ args2 _) = RmtArgClr id oc (args1 ++ args2) role

-- * Make an argument cluster from an existing one.
mkArgClr :: Int -> ObjClr -> ArgClr -> ArgClr
mkArgClr id oc (AdjArgClr _ _ args role) = AdjArgClr id oc args role
mkArgClr id oc (RmtArgClr _ _ args role) = RmtArgClr id oc args role

-- * Returns the a list without the xs.
deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll xs = filter (`notElem` xs)
