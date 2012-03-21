-- Removes old revisions of articles.
module SP.Preprocess.ArticleCleaner where

import Data.Function
import Data.List.Extras.Argmax (argmax)
import Data.Ord

import SP.Cluster
import qualified SP.ByteString as B

-- Stream fusion
import Data.List.Stream
import Prelude hiding ( concat, concatMap, filter, head, length, map, notElem
                      , null, tail, zipWith )

type Sentence = [ObjectCluster]
type SentenceTuple = ((Int,Int),B.ByteString)

cleanArticles :: [Partition] -> [Partition]
cleanArticles partitions = let tuples = toSntTuples partitions
                               groups = toGroups tuples
                               del = nub $ concatMap (delArts tuples) groups
                           in filter ((`notElem` del) . ptnId) partitions

-- | A list of article ids of duplice articles
delArts :: [SentenceTuple] -> [SentenceTuple] -> [Int]
delArts all group = let key = fst . fst
                        duplicates = intersectBy ((==) `on` key) all
                        best = bestInGroup all group
                    in delete best . map key . duplicates $ group

-- | Articles id of the best article in a group
bestInGroup :: [SentenceTuple] -> [SentenceTuple] -> Int
bestInGroup all = let key = length . sntsInArt all . fst . fst
                  in fst . fst . argmax key

sntsInArt :: [SentenceTuple] -> Int -> [SentenceTuple]
sntsInArt all artId = filter (\st -> (fst . fst) st == artId) all

-- | Groups sentences that are the same.
toGroups :: [SentenceTuple] -> [[SentenceTuple]]
toGroups = filter (not . null . tail) 
         . groupBy ((==) `on` snd) 
         . sortBy (comparing snd)

toSntTuples :: [Partition] -> [SentenceTuple]
toSntTuples = let toTuple artId = zipWith (\sId s -> ((artId,sId),s)) [0..]
                  toArts artId ptn = toTuple artId $ toSentences ptn
              in concat . zipWith toArts [0..]

toSentences :: Partition -> [B.ByteString]
toSentences = map toText . groupBy sntEq . oSort . ocs

oSort :: [ObjectCluster] -> [ObjectCluster]
oSort = sortBy $ comparing ocId

sntEq :: ObjectCluster -> ObjectCluster -> Bool
sntEq x y = ((==) `on` (sntId . head . parts)) x y
         && ((==) `on` (artId . head . parts)) x y

toText :: Sentence -> B.ByteString
toText = let toTextList = map $ text . head . parts
         in B.concat . intersperse (B.pack ":") . toTextList

