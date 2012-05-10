{-# LANGUAGE TypeSynonymInstances #-}
module SP.Cluster where

import Data.Function
import Data.Hashable
import Data.Maybe
import Data.Word
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
                                   , samples :: Double
                                   , incSum :: Double
                                   } deriving (Read,Show)

data Part = Part { partId, artId, sntId :: Id
                 , form, pos, lemma, ner, text :: ByteString
                 } deriving (Read,Show)

data ArgumentCluster = ArgumentCluster { acId :: Id
                                       , parMap, chdMap :: ObjectMap
                                       , relMap :: RelationMap
                                       , numArgs :: Double}
                     | D2ArgumentCluster { acFst, acSnd :: ArgumentCluster
                                         } deriving (Read,Show)

instance Eq Partition where (==) = (==) `on` ptnId
instance Eq ObjectCluster where (==) = (==) `on` ocId
instance Eq Part where (==) = (==) `on` partId
instance Hashable ObjectCluster where hash = ocId
instance Hashable Partition where hash = ptnId

instance Eq ArgumentCluster where
  D2ArgumentCluster x y == D2ArgumentCluster z w = x == z && y == w
  ArgumentCluster {acId = id1} == ArgumentCluster {acId = id2} = id1 == id2
  _ == _ = False

instance Hashable ArgumentCluster where 
  hash (D2ArgumentCluster x y) = acId x `combine` acId y
  hash ArgumentCluster {acId = acId} = acId

-- For serialization.
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

parObjIds, chdObjIds, sblObjIds, objIds :: ObjectCluster -> [Id]
parObjIds = concatMap (keys . parMap . fst) . pars 
chdObjIds = concatMap (keys . chdMap . fst) . chdn
sblObjIds = concatMap (keys . chdMap . acSnd . fst) . sbls
objIds o  = parObjIds o ++ chdObjIds o ++ sblObjIds o

-- Sum of incidences of argument clusters in an object cluster.
incidenceSum :: ObjectCluster -> Double
incidenceSum o = sum . snd . unzip $ pars o ++ chdn o ++ sbls o

partTexts :: ObjectCluster -> [String]
partTexts =  map (unpack . text) . parts
