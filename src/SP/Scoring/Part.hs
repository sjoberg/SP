
module SP.Scoring.Part (scoreParts) where

import Data.ByteString (ByteString)
import Data.HashMap.Lazy (HashMap, foldl', fromListWith, intersectionWith)
import SP.Cluster

-- | Mean error of lemma frequencies. O(4*n)
scoreParts :: ObjCluster -> ObjCluster -> Double
scoreParts x y = 1 - errSum / size 
  where
    meanErr w z = abs (w / partLen x - z / partLen y)
    errors = intersectionWith meanErr (counts x) (counts y)
    (size,errSum) = foldl' (\(len,acc) v -> (len + 1, acc + v)) (0,0) errors

-- | Map from lemma to counts.
counts :: ObjCluster -> HashMap ByteString Double
counts = fromListWith (+) . map (flip (,) 1 . lemma) . parts

-- | Number of parts in an object cluster.
partLen :: ObjCluster -> Double
partLen = fromIntegral . length . parts

-- NB. Could use foldr to see if the momentary accumulated value is above the 
-- threshold and terminate earlier, but the intersection will still need to be 
-- traversed to calculate the length of it. Still, since you know the minimum
-- part length, you know the maximum size of the intersection, and can make a 
-- termination guaranteed to be correct, through knowing the minimum correct
-- accumulated value. Still, above threshold cases for lemma equality would be
-- a rear exception, since not many words has the same lemma. Therefore not 
-- much would be gained, since this implies that earlier terminations will be 
-- rear as well. 