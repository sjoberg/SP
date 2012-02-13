{-# LANGUAGE TypeSynonymInstances #-}
module SP.Cluster where

import Data.Function
import Data.Hashable
import Data.Maybe
import SP.ByteString
import Data.IntMap (IntMap,keys)
import Data.HashMap.Lazy (HashMap,fromList)
import Text.Read

data Partition = Partition { ptnId :: Id
                           , ocs :: [ObjectCluster]
                           , acs :: [ArgumentCluster]
                           } deriving (Read,Show)

data ObjectCluster = ObjectCluster { ocId :: Id 
                                   , parts :: [Part]
                                   , pars, chdn, sbls :: IncidenceList
                                   } deriving (Read,Show)

data Part = Part { partId, artId, sntId :: Id
                 , form, pos, lemma, ner, text :: ByteString
                 } deriving (Read,Show)

data ArgumentCluster = ArgumentCluster { acId :: Id
                                       , parMap, chdMap :: ObjectMap
                                       , relMap :: RelationMap} |
                       D2ArgumentCluster { acFst, acSnd :: ArgumentCluster
                                         } deriving (Read,Show)

instance Eq Partition where (==) = (==) `on` ptnId
instance Eq ObjectCluster where (==) = (==) `on` ocId
instance Eq Part where (==) = (==) `on` partId
instance Hashable ObjectCluster where hash = ocId

instance Eq ArgumentCluster where
  D2ArgumentCluster x y == D2ArgumentCluster z w = x == z && y == w
  ArgumentCluster {acId = id1} == ArgumentCluster {acId = id2} = id1 == id2
  _ == _ = False

instance Hashable ArgumentCluster where 
  hash (D2ArgumentCluster x y) = acId x `combine` acId y
  hash ArgumentCluster {acId = acId} = acId

instance (Read k, Hashable k, Ord k, Read a) => Read (HashMap k a) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)
  readListPrec = readListPrecDefault

type Id = Int
type Relation = ByteString
type Incidence = Double
type IncidenceList = [(ArgumentCluster, Incidence)]
type ObjectMap = IntMap Incidence
type RelationMap = HashMap Relation Incidence

parObjIds, chdObjIds, sblObjIds :: ObjectCluster -> [Id]
parObjIds = concatMap (keys.parMap.fst).pars 
chdObjIds = concatMap (keys.chdMap.fst).chdn
sblObjIds = concatMap (keys.chdMap.acSnd.fst).sbls

