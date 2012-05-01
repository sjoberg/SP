module SP.Score.Score where

import SP.Cluster
import Data.Hashable
import Data.IntMap (IntMap)
import Data.List (foldl')

import SP.Score.Math

data OperatorScore = OperatorScore 
  { opScrVal :: Double
  , op :: Operator
  , objScr :: ObjectScore
  , argScrs :: [ArgumentScore]
  } deriving (Show)

data ObjectScore = ObjectScore 
  { o1, o2 :: ObjectCluster
  , partScrVal :: Double
  } deriving (Show)

data ArgumentScore = ArgumentScore 
  { argScrVal :: Double
  , argScrOp :: Operator
  , a1, a2 :: ArgumentCluster
  , rel :: ArgumentRelation
  , i1, i2 :: Double
  } deriving (Show)

data Operator = Merge | Abstract | Child | Parent deriving (Eq, Show)

data ArgumentRelation = ChildArgument 
                      | ParentArgument 
                      | SiblingArgument 
                      deriving (Show)

instance Hashable Operator where
  hash Merge    = 1
  hash Abstract = 2
  hash Child    = 3
  hash Parent   = 4

instance Eq ArgumentScore where 
  as1 == as2 = a1 as1 == a1 as2 && a2 as1 == a2 as2

-- | Object reference map accessor for an argument cluster.
objRefMap :: ArgumentScore -> ArgumentCluster -> IntMap Incidence
objRefMap score = case rel score of ParentArgument  -> parMap
                                    ChildArgument   -> chdMap
                                    SiblingArgument -> chdMap . acSnd

type Threshold = Double
type Weight = Double

data ParamSet = ParamSet {tm, ta, tc :: Threshold, wm, wa, wc :: Weight}

-- Sum of incidences of argument clusters in an object cluster.
incidenceSum :: ObjectCluster -> Double
incidenceSum o = sum . snd . unzip $ pars o ++ chdn o ++ sbls o

-- | Match mark.
matchMark :: Double -> Double -> [ArgumentScore] -> Double
matchMark iSum1 iSum2 = let f, g :: ArgumentScore -> Double
                            f s = argScrVal s * i1 s / iSum1
                            g s = argScrVal s * i2 s / iSum2
                            sum = foldl' (\(t,u) s -> (t + f s, u + g s)) (0,0)
                        in uncurry hmean2 . sum 

