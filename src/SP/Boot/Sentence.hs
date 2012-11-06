
module SP.Boot.Sentence where

import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import SP.Cluster

-- | Create sentences from object clusters.
toSentences :: [ObjCluster] -> [[ObjCluster]]
toSentences = groupBy sameSentence . sortBy (comparing objClusterId)
  where
    sameSentence c1 c2 =
        (documentId . head . parts) c1 == (documentId . head . parts) c2 &&
        (sentenceId . head . parts) c1 == (sentenceId . head . parts) c2