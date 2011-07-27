module Cluster where
import Data.List

data ObjCluster = ObjCluster {uid::Int, parts::[Part], argClusters::[ArgCluster]} deriving (Show)

instance Eq ObjCluster where
  c1 == c2 = uid c1 == uid c2

data ArgCluster = SibCluster Int [Dep]
                | ParCluster Int [Dep] 
                | ChdCluster Int [(Dep,Dep)] 
                deriving (Show)

instance Eq ArgCluster where
  SibCluster id1 _ == SibCluster id2 _ = id1 == id2
  ParCluster id1 _ == ParCluster id2 _ = id1 == id2
  ChdCluster id1 _ == ChdCluster id2 _ = id1 == id2
  _                == _                = False

data Part = Part {form :: String, lemma :: String, pos :: String, text :: String} deriving (Show)

data Dep = Dep {rel::String, obj::Int} deriving (Show) -- Relation, destination object

instance Eq Dep where
  Dep rel1 obj1 == Dep rel2 obj2 = (rel1 == rel2) && (obj1 == obj2)




