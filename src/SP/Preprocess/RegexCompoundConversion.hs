module SP.Preprocess.RegexCompoundConversion (toString, posSet, posIdMap) where

import Data.Char
import Data.Hashable
import Data.List (nub)
import Data.Maybe
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap, fromList)
import SP.Cluster
import SP.ByteString

-- Converts a byte string to a one character string.
toString :: HashMap ByteString Int -> ByteString -> String
toString map byteString = [chr.fromJust $ HashMap.lookup byteString map]

-- Generates a list with repetition of POS tags.
posSet :: [Partition] -> [ByteString]
posSet = nub . map (pos . part) . concatMap ocs

-- Generates a ID to POS map from a list of POS tags.
idPosMap :: [ByteString] -> HashMap Int ByteString
idPosMap = fromList . zip indices

-- Generates a POS to ID map from a list of POS tags.
posIdMap :: [ByteString] -> HashMap ByteString Int
posIdMap = fromList . flip zip indices

-- | The part of an object cluster.
part :: ObjectCluster -> Part
part = head . parts

-- | Allowed indices.
indices :: [Int]
indices = [65..90] ++ [97..122]

