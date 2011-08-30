module SP.Cluster where

import Data.ByteString.Char8 (ByteString, unpack)
import Data.Hashable
import Data.List

-- | Article.
data Art = Art {aId::Int, aSnts::[Snt], aTitle, aText::ByteString} 
-- | Sentence.
data Snt = Snt {sId::Int, sArt::Art, sParts::[Part]} deriving (Show)
-- | Part.
data Part = Part {pId::Int, pSnt::Snt, form, lemma, pos, word::ByteString, 
                  parDeps, chdDeps::[Dep]}
-- | Dependency.
data Dep = Dep {dRel::ByteString, gov, dpt::Part} deriving (Show)
-- | Object cluster.
data ObjClr = ObjClr {ocId::Int, ocParts::[Part], parArgClrs, chdArgClrs, 
                      sblArgClrs::[ArgClr]}
-- | Argument. Adjacent or remote.
data AdjArg = AdjArg {rel::ByteString, obj::ObjClr}
data RmtArg = RmtArg {intRel,rmtRel::ByteString, intObj,rmtObj::ObjClr} 
-- | Argument cluster.
data ArgClr = AdjArgClr {aacId::Int, aacObj::ObjClr, aacArgs::[AdjArg], 
                         aacRole::Role} 
            | RmtArgClr {racId::Int, racObj::ObjClr, racArgs::[RmtArg], 
                         racRole::Role} 
data Role = Par | Chd | Sbl deriving (Show, Eq)

-- General accessor methods for argument clusters.
acId :: ArgClr -> Int
acId AdjArgClr {aacId = id} = id
acId RmtArgClr {racId = id} = id
acRole :: ArgClr -> Role
acRole AdjArgClr {aacRole = role} = role
acRole RmtArgClr {racRole = role} = role

-- Equality.
instance Eq Art where
  a1 == a2 = aId a1 == aId a2

instance Eq Snt where
  s1 == s2 = sId s1 == sId s2 && sArt s1 == sArt s2

instance Eq Part where
  p1 == p2 = pId p1 == pId p2 && pSnt p1 == pSnt p2
  
instance Eq ObjClr where
  c1 == c2 = ocId c1 == ocId c2

instance Eq ArgClr where
  -- Must have same id, object cluster, and role.
  AdjArgClr id1 oc1 _ r1 == AdjArgClr id2 oc2 _ r2 = 
    id1 == id2 && oc1 == oc2 && r1 == r2
  RmtArgClr id1 oc1 _ r1 == RmtArgClr id2 oc2 _ r2 = 
    id1 == id2 && oc1 == oc2 && r1 == r2

-- Show.
instance Show Part where
  show p = unpack (word p) ++ " " ++ unpack (form p)

instance Show ObjClr where
  show oc = "\n" ++ id ++ parts ++ parArgs ++ chdArgs ++ sblArgs
    where
      id = fuse ["ID: ", show (ocId oc)]
      parts = fuse ["Parts: ", show (ocParts oc)]
      parArgs = fuse ["ParArgClrs: ", show (parArgClrs oc)] 
      chdArgs = fuse ["ChdArgClrs: ", show (chdArgClrs oc)] 
      sblArgs = fuse ["SblArgClrs: ", show (sblArgClrs oc)] 

fuse strs = concat strs ++ "\n"

instance Show ArgClr where
  show AdjArgClr {aacArgs = args} = show args
  show RmtArgClr {racArgs = args} = show args

instance Show AdjArg where
  show (AdjArg rel obj) = unpack rel ++ " " ++ show (ocId obj) ++ " " ++ 
    show (ocParts obj)

instance Show RmtArg where
  show (RmtArg intRel rmtRel intObj rmtObj) = 
    unpack intRel ++ " " ++ show (ocId intObj) ++ " " ++ 
      show (ocParts intObj) ++ "\n" ++
      unpack rmtRel ++ " " ++ show (ocId rmtObj) ++ " " ++ 
      show (ocParts rmtObj)

instance Show Art where
  show Art {aId = id, aTitle = title, aText = text} = 
    fuse ["ID: ", show id] ++ fuse ["Title: ", unpack title] ++ fuse ["Text: ", unpack text]

-- Hashable.
instance Hashable Art where
  hash = aId

instance Hashable Snt where
  hash snt = sId snt `combine` hash (sArt snt)

instance Hashable Part where
  hash part = pId part `combine` hash (pSnt part)
  
instance Hashable ObjClr where
  hash = ocId

