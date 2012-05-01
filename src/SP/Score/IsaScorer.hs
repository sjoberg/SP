module SP.Score.IsaScorer (isaScore) where

import Data.Function (on)
import Data.IntMap (difference, elems, intersectionWith)

import SP.Cluster
import SP.Score.Math
import SP.Score.Score ( ArgumentScore, OperatorScore, a1, a2, objScr
                      , objArgScrs, objRefMap)

isaScore :: OperatorScore -> OperatorScore
isaScore operatorScore = 
  let argScores = objArgScrs . objScr $ operatorScore
    in operatorScore

isaChildScore :: Threshold -> [ArgumentScore] -> Double
isaChildScore t scores = let passed = filter passes scores
                             passes s = case argScrOp s of Child -> s >= t
                                                           Merge -> s >= t
                                                           _     -> False

                         in sum . map ()

updateScore :: ArgumentScore -> Operator -> Double -> ArgumentScore
updateScore score = score

-- Abstract score.
abstract :: Distribution -> ArgumentScore -> ArgumentScore
abstract (Distribution x y z) score = score { argScrOp = Abstract
                                            , argScrVal = 1 - ca3 x y z}

-- ISA score, parent or child.
isa score (Distribution x y z) | x > y && z > x = set Child (hmean2 v w)
                               | x > y          = set Child (hmean2 v (1 - w))
                               | y > x && x > z = set Parent (hmean2 v w)
                               | x > z          = set Parent (hmean2 (1 - v) w)
                               | otherwise      = EmptyScore

  where set operator marks = score {argScrOp = operator, argScrVal = marks}

        v = ca2 x y
        w = ca2 x z



-- Distribution of reference matches.
distribution :: ArgumentScore -> Distribution
distribution score@ArgumentScore {a1 = ai, a2 = aj} = 
  let mi = objRefMap score ai
      mj = objRefMap score aj

      -- Reference match.
      match = sum . elems $ intersectionWith ((+) `on` (/2)) mi mj
      
      -- References in ai, but not in aj, resp. in aj, but not in ai.
      noMatch1 = sum . elems $ difference mi mj
      noMatch2 = sum . elems $ difference mj mi

  in Distribution match noMatch1 noMatch2 

data Distribution = Distribution {match, noMatch1, noMatch2 :: Incidence}

