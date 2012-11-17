
module Constructors where

import Data.ByteString.Char8 (pack)
import Data.HashMap.Lazy (empty, singleton)
import Database.MongoDB
import SP.Cluster
import SP.Config

-- | Create a new object cluster.
newObjCluster :: Int            -- ^ Id
              -> String         -- ^ Word 
              -> String         -- ^ Lemma
              -> String         -- ^ POS
              -> [ArgCluster]   -- ^ Parents
              -> [ArgCluster]   -- ^ Children
              -> [ArgCluster]   -- ^ Siblings
              -> ObjCluster
newObjCluster clusterId pWord pLemma pPos pars chdn sbls = ObjCluster
    { objClusterId = clusterId
    , numSamples = 1
    , parts = [newPart clusterId pWord pLemma pPos]
    , hypernyms = []
    , hyponyms = []
    , parents = pars
    , children = chdn
    , siblings = sbls
    }

-- | Create a new part.
newPart :: Int -> String -> String -> String -> Part
newPart pId pWord pLemma pPos = Part
    { partId = pId
    , documentId = -1
    , sentenceId = -1
    , lemma = pack pLemma
    , pos = pack pPos
    , word = pack pWord
    , ner = pack ""
    }

-- | Create a new parent and child argument cluster pair.
newParChdPair :: Int -> Int -> Int -> Int -> String -> (ArgCluster, ArgCluster)
newParChdPair paId1 caId2 pcId1 ccId2 rel = (pa, ca)
  where
    pa = ArgCluster { argClusterId = paId1
                    , frequency = 1
                    , numArgs = 1
                    , isaParents = []
                    , isaChildren = []
                    , objFrequency = singleton pcId1 1
                    , relFrequency = singleton (pack rel) 1
                    , subObjFrequency = empty
                    , subRelFrequency = empty
                    }
    ca = ArgCluster { argClusterId = caId2
                    , frequency = 1
                    , numArgs = 1
                    , isaParents = []
                    , isaChildren = []
                    , objFrequency = singleton ccId2 1
                    , relFrequency = singleton (pack rel) 1
                    , subObjFrequency = empty
                    , subRelFrequency = empty
                    }

-- | Default configuration to be used for tests.
defCfg :: Config
defCfg = Config 
    { minSmpSize = 5    -- Truncate processing of clusters
    , artSize    = 50   -- Number of articles to process
    , ignRels    = []   -- Relations to ignore
    , ignPosTags = []   -- POS tags to ignore
    , compoundFile = "en.compound"
    , lemmaIterHt  = 6.0
    , lemmaIterLt  = 3.0
    , posCatIterHt = 6.0
    , posCatIterLt = 3.0
    , tm  = 0.5
    , ta  = 0.5
    , tc  = 0.5
    , tma = 0.5
    , taa = 0.5
    , tca = 0.5
    , wm  = 0.0
    , wa  = 0.0
    , wc  = 0.0
    , tsyn = 0.25
    , useHyponymys = False
    , mongoHost = Host "127.0.0.1"   -- MongoDB host
                $ PortNumber 27017
    }