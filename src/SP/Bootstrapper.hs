{-# LANGUAGE OverloadedStrings #-}
module SP.Bootstrapper (bootstrap) where

import SP.Cluster
import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Data.List (elemIndex)
import Data.Maybe

-- Get the news items from Mongo, where cb is the callback function that will
-- be called upon once the initial clustering has been created.
bootstrap cb = do
  pipe <- runIOE $ connect (host "127.0.0.1")
  e <- access pipe master "articles" (reutersMergers >>= mkArts cb)
  close pipe
  print e

-- Find Reuters Mergers news items.
reutersMergers = rest =<< find (select q "reuters.mergers")
  where 
    q = ["status" =: u"ok", "sentences" =: ["$exists" =: True]]

-- Create articles, then supply all articles to the callback function.
mkArts cb arts = liftIO $ cb [mkArt tks id art | (id, art) <- zip [1..] arts]
  where
    tks = getTks arts

-- Get all tokens from the specified articles.
getTks :: [Document] -> [Document]
getTks arts = concatMap tks $ getSnts arts
  where 
    tks :: Document -> [Document]
    tks snt = typed (valueAt "tokens" snt)

-- Get all sentences from the specified articles.
getSnts :: [Document] -> [Document]
getSnts arts = concatMap snts arts
  where
    snts :: Document -> [Document]
    snts art = typed (valueAt "sentences" art)

-- Create an article.
mkArt :: [Document] -> Int -> Document -> Article
mkArt tks id art = Article id (map (mkSentence tks) sentences) title text
  where
    val lbl = valueAt lbl art
    text = typed (val "content")
    title = typed (val "title")
    sentences :: [Document]
    sentences = typed (val "sentences")

-- Create a sentence.
mkSentence :: [Document] -> Document -> Sentence
mkSentence tks doc = Sentence objClusters
  where
    -- The tokens of the sentence.
    sntTks :: [Document]
    sntTks = typed (valueAt "tokens" doc)
    -- The dependencies of the sentence.
    sntDeps :: [Document]
    sntDeps = typed (valueAt "deps" doc)
    -- The object clusters of the sentence.
    objClusters :: [ObjCluster]
    objClusters = map (mkObjCluster tks objClusters sntDeps sntTks) sntTks
                   
-- Create an object cluster. Needs all the object clusters of the sentence it
-- exists in (including itself), in order to be created.
-- docDeps are the documents representing the dependencies in the sentence 
-- this object cluster is created in.
-- ocs are the objectClusters spawned from that same sentence this created 
-- object cluster is spawned from.
mkObjCluster :: [Document] -> [ObjCluster] -> [Document] -> [Document] -> Document -> ObjCluster                 
mkObjCluster tks ocs sntDeps sntTks tk = objCluster
  where 
    -- Create the object cluster.
    objCluster = ObjCluster oid [mkPart tk] argClusters
    -- The id of the object cluster.
    oid = fndIdx tk tks
    -- Get documents representing parent and child dependencies.
    pars, chdn :: [Document]
    pars = typed (valueAt "dept_deps" tk)
    chdn = typed (valueAt "gov_deps" tk)
    -- Create parents, children and sibling dependencies.
    parDeps, chdDeps :: [Dep]
    parDeps = map (mkDep "gov_index" ocs) pars 
    chdDeps = map (mkDep "dept_index" ocs) chdn
    sblDepPairs :: [(Dep, Dep)]
    sblDepPairs = concatMap toSblDepPair pars
      where 
        toSblDepPair par = [(mkDep "dept_index" ocs sbl, docToDep par pars parDeps) | sbl <- getChdn par]
        -- Get child dependencies from a parent.
        getChdn par = filter flt sntDeps
        -- Filter dependencies that are children to the given parent 
        -- (excluding dependencies pointing to this cluster)
        flt p = gi == fndIdx p sntDeps && gi /= fndIdx tk sntTks
          where gi = govIdx p
    -- Look up a created dependency from a document.
    docToDep :: Document -> [Document] -> [Dep] -> Dep 
    docToDep doc docs deps = deps !! fndIdx doc docs
    -- Create argument clusters.
    argClusters = chdClusters ++ parClusters ++ sblClusters
      where
        chdClusters = [ChdCluster i objCluster [c] | (c,i) <- zip chdDeps [1..]]
        parClusters = [ParCluster i objCluster [p] | (p,i) <- zip parDeps [1..]]
        sblClusters = [SblCluster i objCluster [(s,p)] | ((s,p),i) <- zip sblDepPairs [1..]]

-- Create a dependency. idxName, denotes which index that will be used to 
-- lookup the object cluster, from the document representing the dependency.
-- Use "gov_index" for parents, or "dept_index" for children.
mkDep :: Label -> [ObjCluster] -> Document -> Dep 
mkDep idxName objClusters doc = Dep rel obj
  where
    rel :: String
    rel = typed (valueAt "rel" doc)
    objIdx :: Int
    objIdx = typed (valueAt idxName doc)
    obj :: ObjCluster
    obj = objClusters !! objIdx

-- Create a part.
mkPart :: Document -> Part
mkPart tk = Part form lemma pos text
  where
    getVal :: Label -> String
    getVal lbl = typed (valueAt lbl tk)
    pos = getVal "pos"
    lemma = getVal "lemma"
    text = getVal "text"
    form = pos ++ ":" ++ lemma

-- Utility functions
fndIdx :: (Eq a) => a -> [a] -> Int
fndIdx x xs = fromJust $ elemIndex x xs
govIdx :: Document -> Int
govIdx dep = typed $ valueAt "gov_index" dep
dptIdx :: Document -> Int
dptIdx dep = typed $ valueAt "dept_index" dep

