-- | Data types for scores and operators.
module SP.Scoring.Score where

import Data.List (intersect)
import SP.Cluster

data Score = Score
    { scoreValue        :: Double       -- ^ Score value.
    , scoreOp           :: Operator     -- ^ Score operator.
    , objLeft, objRight :: ObjCluster   -- ^ Object clusters for the score.
    , argScores         :: [ArgScore]   -- ^ Argument scores.
    } deriving (Show)

data ArgScore = ArgScore
    { argLeft, argRight :: ArgCluster   -- ^ Argument clusters.
    , argScoreOp        :: Operator     -- ^ Operator.
    , argScoreValue     :: Double       -- ^ Score value.
    } deriving (Show)

data Operator = Merge | Abstract | Parent | Child deriving (Eq, Show)

class Scorable s where independent :: s -> s -> Bool

instance Scorable ArgScore where
    x `independent` y = null $ [argLeft x, argRight x] `intersect` [argLeft y, argRight y]

instance Scorable Score where
    x `independent` y = null $ [objLeft x, objRight x] `intersect` [objLeft y, objRight y]
