-- | Module for domain.
module SP.Cluster where

import Data.ByteString.Char8 (ByteString)
import Data.HashMap.Lazy (HashMap, null)
import Data.Function (on)
import Data.Hashable (Hashable, hash)

-- | An object cluster. Represents a set of synonyms.
data ObjCluster = ObjCluster
    { objClusterId                  :: Int              -- ^ Id
    , numSamples                    :: Int              -- ^ Number of samples
    , parts                         :: [Part]           -- ^ Parts
    , hypernyms, hyponyms           :: [ObjCluster]     -- ^ ISA relations
    , parents, children, siblings   :: [ArgCluster]     -- ^ Argument clusters
    }

-- | An argument cluster. Represents a set of arguments.
data ArgCluster = ArgCluster
    { argClusterId                  :: Int                          -- ^ Id
    , frequency                     :: Double                       -- ^ Frequency of use
    , numArgs                       :: Int                          -- ^ Number of arguments
    , isaParents, isaChildren       :: [ArgCluster]                 -- ^ ISA relations
    , objFrequency, subObjFrequency :: HashMap Int Double           -- ^ Object frequency
    , relFrequency, subRelFrequency :: HashMap ByteString Double    -- ^ Relation frequency
    }

-- | A part. Represents a token with annotations.
data Part = Part
    { partId                        :: Int          -- ^ Id
    , documentId, sentenceId        :: Int          -- ^ Location
    , lemma, pos, word, ner         :: ByteString   -- ^ Token annotations
    }
    
instance Eq ObjCluster where (==) = (==) `on` objClusterId
instance Eq ArgCluster where (==) = (==) `on` argClusterId
instance Eq Part where (==) = (==) `on` partId

instance Hashable ObjCluster where hash = objClusterId
instance Hashable ArgCluster where hash = argClusterId

-- | Get all argument clusters from an object cluster.
allArgClusters :: ObjCluster -> [ArgCluster]
allArgClusters c = parents c ++ children c ++ siblings c

-- | True if the argument cluster only points to a nearest neighbour.
nearestNeighbour :: ArgCluster -> Bool
nearestNeighbour = Data.HashMap.Lazy.null . subObjFrequency

-- | The most distant object cluster in the argument cluster.
endObjMap :: ArgCluster -> HashMap Int Double
endObjMap x = if nearestNeighbour x then objFrequency x else subObjFrequency x