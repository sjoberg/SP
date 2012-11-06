-- | MongoDB bootstrap.
module SP.Boot.MongoDB where

import qualified Data.Text as Text
import Control.Monad.Trans (liftIO)
import Database.MongoDB
import Data.ByteString.Char8 (pack)
import Data.HashMap.Lazy (empty, singleton)
import Data.List (elemIndex)
import SP.Cluster
import SP.Config
import Data.Maybe (fromJust)

-- | Unicode function.
u :: String -> Text.Text
u = Text.pack

-- | Get typed attribute value from a document.
getVal :: Val a => String -> Document -> a
getVal name = typed . valueAt (u name)

-- | Bootstrap function.
bootstrap :: ([ObjCluster] -> IO ()) -> IO ()
bootstrap callback = do
    config <- getConfig
    pipe <- runIOE . connect $ mongoHost config
    let order = [u"generated" =: (1 :: Int)]
        collection = u"reuters.mergers"
        sel = [u"status" =: u"ok", u"sentences" =: [u"$exists" =: True]]
        get = rest =<< find (select sel collection) {sort = order}
    e <- access pipe master (u"articles") (get >>= mkDomain config callback)
    close pipe
    print e

-- | Make the domain and send to callback function.
mkDomain :: Config -> ([ObjCluster] -> IO ()) -> [Document] -> Action IO ()
mkDomain config callback = liftIO . callback . mkObjClusters config . take (artSize config)

-- | Make object clusters (and thereby argument clusters). Make less complex.
mkObjClusters :: Config -> [Document] -> [ObjCluster]
mkObjClusters config = objs . foldr procArticle (1,1,1,[])
  where
    objs (_,_,_,x) = x
    -- Process an article.
    procArticle :: Document -> (Int,Int,Int,[ObjCluster]) -> (Int,Int,Int,[ObjCluster])
    procArticle article (artId,objClusterArtStartId,argClusterArtStartId,doneArt) =
        (artId + 1, nObjClusterArtStartId, nArgClusterArtStartId, nDoneArt)
      where
        articles = getVal "sentences" article :: [Document]
        -- Start tuple for the processing sentences fold.
        startTuple = (1,objClusterArtStartId,argClusterArtStartId,doneArt)
        -- Fold.
        (_,nObjClusterArtStartId,nArgClusterArtStartId,nDoneArt) = foldr procSentence startTuple articles
        -- Process a sentence.
        procSentence :: Document -> (Int,Int,Int,[ObjCluster]) -> (Int,Int,Int,[ObjCluster])
        procSentence sentence (sntId,objClusterSntStartId,argClusterSntStartId,doneSnt) = 
            ( sntId + 1
            , objClusterSntStartId + length objClusters
            , argClusterSntStartId + 2 * length dependencies + sum (map (length . siblings) objClusters)
            , doneSnt ++ objClusters
            )
          where
            -- Filtered dependencies.
            filterDeps = filter $ \dep -> rel dep `notElem` ignRels config
            dependencies = filterDeps (getVal "deps" sentence :: [Document])
            -- True if the token with the given index functions as the given role in the given dependency.
            is :: String -> Int -> Document -> Bool
            is role tokenIdx dep = (getVal role dep :: Int) == tokenIdx
            -- Get the relation of a dependency.
            rel dep = getVal "rel" dep :: String
            -- Get dependencies given token index. 
            parentDependenciesFor tokenIdx = filter (is "dept_index" tokenIdx) dependencies
            childDependenciesFor tokenIdx = filter (is "gov_index" tokenIdx) dependencies
            argClusterIdFrom dependency = argClusterSntStartId 
                                        + fromJust (elemIndex dependency dependencies)
            -- Create a parent argument cluster.
            mkParentArgCluster dep = mkArgCluster (argClusterIdFrom dep) obj (-1) (rel dep) []
              where
                obj = objClusterSntStartId + (getVal "gov_index" dep :: Int)
            -- Create a child argument cluster.
            mkChildArgCluster dep = mkArgCluster (argClusterIdFrom dep) obj (-1) (rel dep) []
              where
                obj = objClusterSntStartId + (getVal "dept_index" dep :: Int)
            -- Create sibling argument clusters.
            mkSiblingArgClusters dep = map mkSiblingArgCluster subDeps
              where
                -- Dependent index.
                dept = getVal "gov_index" dep :: Int
                -- Object cluster index.
                obj = objClusterSntStartId + dept
                -- Child dependencies for this parent.
                subDeps = childDependenciesFor dept
                -- Create the sibling cluster.
                mkSiblingArgCluster subDep = mkArgCluster (argClusterIdFrom dep) obj subObj (rel dep) (rel subDep)
                  where
                    subObj = objClusterSntStartId + (getVal "dept_index" subDep :: Int)
            -- Create object clusters from tokens of this sentence.
            tokens = getVal "tokens" sentence :: [Document]
            objClusters = zipWith mkObjCluster [objClusterSntStartId ..] tokens
            -- Make nearest neighbor argument cluster.
            mkArgCluster :: Int -> Int -> Int -> String -> String -> ArgCluster
            mkArgCluster clusterId obj subObj depRel subDepRel = ArgCluster
                { argClusterId = clusterId
                , frequency = 1
                , numArgs = 1
                , isaParents = []
                , isaChildren = []
                , objFrequency = singleton obj 1
                , subObjFrequency = if subObj < 0 then empty else singleton subObj 1
                , relFrequency = singleton (pack depRel) 1
                , subRelFrequency = if null subDepRel then empty else singleton (pack subDepRel) 1
                } 
            -- Create object cluster and part.
            mkObjCluster clusterId token = ObjCluster
                { objClusterId = clusterId
                , parts = [Part
                    { partId = clusterId
                    , documentId = artId
                    , sentenceId = sntId
                    , word = pack $ getVal "text" token
                    , lemma = pack $ getVal "lemma" token
                    , pos = pack $ getVal "pos" token
                    , ner = pack $ getVal "ner" token
                    }]
                , numSamples = 1
                , hypernyms = []
                , hyponyms = []
                , parents = map mkParentArgCluster (parentDependenciesFor tokenIdx)
                , children = map mkChildArgCluster (childDependenciesFor tokenIdx)
                , siblings = concatMap mkSiblingArgClusters (parentDependenciesFor tokenIdx) 
                } where tokenIdx = clusterId - objClusterSntStartId -- Token index.
