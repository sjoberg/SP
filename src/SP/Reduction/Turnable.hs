-- | Turn scores and operators into their opposites.
module SP.Reduction.Turnable where

import SP.Scoring.Score

class Turnable t where
    turn :: t -> t

instance Turnable Score where
    turn score = score
        { objLeft = objRight score
        , objRight = objLeft score
        , argScores = map turn (argScores score)
        , scoreOp = turn (scoreOp score)
        }

instance Turnable ArgScore where
    turn score = score 
        { argLeft = argRight score
        , argRight = argLeft score
        , argScoreOp = turn (argScoreOp score)
        }

instance Turnable Operator where
    turn Child = Parent
    turn Parent = Child
    turn x = x