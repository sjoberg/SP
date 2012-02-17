{-# LANGUAGE BangPatterns #-}
module SP.Score.Score where

import SP.Cluster

data OperatorScore = OperatorScore 
  { opScrVal :: Double
  , op :: Operator
  , objScr :: ObjectScore
  } deriving (Show)

data ObjectScore = ObjectScore 
  { o1, o2 :: ObjectCluster
  , partScrVal :: Double
  , objArgScrs :: [ArgumentScore]
  } deriving (Show)

data ArgumentScore = ArgumentScore 
  { argScrVal :: Double
  , a1, a2 :: ArgumentCluster
  , i1, i2 :: Double
  } deriving (Show)

data Operator = Merge | Abstract | Child | Parent deriving (Show)

instance Eq ArgumentScore where 
  as1 == as2 = a1 as1 == a1 as2 && a2 as1 == a2 as2
