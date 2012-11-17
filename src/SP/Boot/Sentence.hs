
module SP.Boot.Sentence where

import Data.Function (on)
import Data.List (groupBy, intersect, nubBy, sortBy)
import Data.Ord (comparing)
import SP.Cluster
import Data.ByteString.Char8 (ByteString, intercalate, pack)

-- | Create sentences from object clusters.
toSentences :: [ObjCluster] -> [[ObjCluster]]
toSentences = groupBy (\x y -> x `sameDocument` y && x `sameSentence` y) 
            . sortBy (comparing objClusterId) 

-- | Create sentences divided into articles from object clusters.
toArticles :: [ObjCluster] -> [[[ObjCluster]]]
toArticles = groupBy (sameDocument `on` head) . toSentences

-- | Remove duplicate articles.
trim :: [ObjCluster] -> [ObjCluster]
trim = let artEq x y = not . null $ (intersect `on` map sentenceToText) x y
       in concat . concat . nubBy artEq . toArticles

sameDocument, sameSentence :: ObjCluster -> ObjCluster -> Bool
sameDocument = (==) `on` (documentId . head . parts)
sameSentence = (==) `on` (sentenceId . head . parts)

-- | Sentence to text.
sentenceToText :: [ObjCluster] -> ByteString
sentenceToText = intercalate (pack " ") . map (word . head . parts)