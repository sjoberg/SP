module SP.Cluster where

import Data.List

data Article = Article {aid::Int, sentences::[Sentence], title::String, text::String} deriving (Show)
data Sentence = Sentence {objClusters::[ObjCluster]} deriving (Show)

data ObjCluster = ObjCluster {uid::Int, parts::[Part], argClusters::[ArgCluster]}

instance Eq ObjCluster where
  c1 == c2 = uid c1 == uid c2

instance Show ObjCluster where
  show oc = show $ parts oc

data ArgCluster = ChdCluster Int ObjCluster [Dep]
                | ParCluster Int ObjCluster [Dep] 
                | SblCluster Int ObjCluster [(Dep,Dep)] 
                deriving (Show)

instance Eq ArgCluster where
  SblCluster id1 oc1 _ == SblCluster id2 oc2 _ = id1 == id2 && oc1 == oc2
  ParCluster id1 oc1 _ == ParCluster id2 oc2 _ = id1 == id2 && oc1 == oc2
  ChdCluster id1 oc1 _ == ChdCluster id2 oc2 _ = id1 == id2 && oc1 == oc2
  _                == _                = False

data Part = Part {form :: String, lemma :: String, pos :: String, word :: String}
instance Show Part where
  show p = show $ form p

data Dep = Dep {rel::String, obj::ObjCluster} deriving (Show) -- Relation, destination object

instance Eq Dep where
  Dep rel1 obj1 == Dep rel2 obj2 = (rel1 == rel2) && (obj1 == obj2)

objClustersFromArts :: [Article] -> [ObjCluster]
objClustersFromArts arts = concatMap objClusters $ concatMap sentences arts

