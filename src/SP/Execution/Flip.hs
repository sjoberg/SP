
module SP.Execution.Flip where

import SP.Scoring.Score

-- | Flip a score.
flipScore :: Score -> Score
flipScore score = score
    { objLeft = objRight score
    , objRight = objLeft score
    , argScores = map flipArgScore (argScores score)
    , scoreOp = flipOp (scoreOp score)
    }

-- | Flip an argument score.
flipArgScore :: ArgScore -> ArgScore
flipArgScore score = score 
    { argLeft = argRight score
    , argRight = argLeft score
    , argScoreOp = flipOp (argScoreOp score)
    }

-- | Flip an operator.
flipOp :: Operator -> Operator
flipOp Child = Parent
flipOp Parent = Child
flipOp x = x