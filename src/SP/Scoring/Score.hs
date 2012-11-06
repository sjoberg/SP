-- | Data types for scores and operators.
module SP.Scoring.Score where

import Data.List (intersect)
import SP.Cluster

data Score = Score
    { objLeft, objRight :: ObjCluster   -- ^ Object clusters for the score.
    , scoreOp           :: Operator     -- ^ Score operator.
    , scoreValue        :: Double       -- ^ Score value.
    , argScores         :: [ArgScore]   -- ^ Argument scores.
    }

data ArgScore = ArgScore
    { argLeft, argRight :: ArgCluster   -- ^ Argument clusters.
    , argScoreOp        :: Operator     -- ^ Operator.
    , argScoreValue     :: Double       -- ^ Score value.
    }

data Operator = Merge | Abstract | Parent | Child deriving Eq

class Scorable s where independent :: s -> s -> Bool

instance Scorable ArgScore where
    x `independent` y = null $ [argLeft x, argRight x] `intersect` [argLeft y, argRight y]

instance Scorable Score where
    x `independent` y = null $ [objLeft x, objRight x] `intersect` [objLeft y, objRight y]
