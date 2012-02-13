module SP.Score.Object where

import Data.Function (on)
import Data.List.Stream -- (foldl', intersect, nub)
import SP.Cluster
import SP.Score.Argument
import SP.Score.Math
import SP.Score.Score
import Prelude hiding (length,map,null)

-- | Scores an object cluster.
objectScore :: ObjectCluster -> ObjectCluster -> ObjectScore
objectScore oi oj = ObjectScore oi oj value argumentScores
  where 
  value = let inc form forms = 1 / (fromIntegral.length) forms * occurence form forms
              occurence f1 = foldl' (\t f2 -> if f1 == f2 then t + 1 else t) 0
              formSeqi = map form $ parts oi
              formSeqj = map form $ parts oj
              formSet = nub $ intersect formSeqi formSeqj
              deltaSum sum f = sum + abs(((-) `on` inc f) formSeqi formSeqj)
              result = 1 - 1 / cardinality formSet * foldl' deltaSum 0 formSet
          in if null formSet then 0 else result
  argumentScores = bestArgumentScores oi oj

-- | Independence of argument scores.
isIndependentOf :: ObjectScore -> ObjectScore -> Bool
isIndependentOf ObjectScore {o1 = oi, o2 = oj} ObjectScore {o1 = ok,o2 = ol} =
  oi /= ok && oj /= ol
