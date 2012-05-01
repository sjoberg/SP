module SP.Print where

import Control.Arrow
import Control.Monad.Trans (liftIO)
import Data.Function
import Data.List
import Data.Ord
import SP.ByteString
import SP.Cluster
import SP.Config
import SP.Bootstrap.MongoDB
import SP.Merge
import SP.Preprocess.ArticleCleaner (cleanArticles)
import SP.Preprocess.Compound
import SP.Preprocess.PosCompound
import SP.Preprocess.Preprocess
import SP.Preprocess.RegexCompound
import SP.Score.Score
import SP.Score.Scorer
import Text.Printf (printf)
import System.Exit
import Data.Maybe (fromJust)

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)

-- | Print text of all partitions.
printText :: [Partition] -> IO ()
printText = mapM_ $ putStrLn . unwords . toText . sort . ocs
  where toText = map $ unpack . text . head . parts
        sort = sortBy (comparing ocId)

-- | Articles in all partitions.
toArticles :: [Partition] -> [[String]]
toArticles = map toSentences

-- | Partitions to article id - sentence id - text tuples.
toSntTuples :: [Partition] -> [((Int,Int),String)]
toSntTuples = let toTuple artId    = zipWith (\sId s -> ((artId,sId),s)) [0..]
                  toArts artId ptn = toTuple artId $ toSentences ptn
              in Prelude.concat . zipWith toArts [0..]

-- | Partition to list of words.
toSentences :: Partition -> [String]
toSentences = let sort = sortBy $ comparing ocId
                  toText = unwords . map (unpack . text . head . parts)
                  sntEq = (==) `on` (sntId . head . parts)
              in map toText . groupBy sntEq . sort . ocs

-- | Object cluster parts to a string list.
textParts :: ObjectCluster -> [String]
textParts oc = map display $ parts oc

-- | Print a part.
display part = intercalate "-" $ map ($ part) [ unpack . form
                                              , show . artId
                                              , show . sntId ]

-- | Print an operator score.
printScore :: (Part -> String) ->  OperatorScore -> IO ()
printScore snt score = do
  print (op score)
  print (opScrVal score)
--mapM_ printArgScore . argScrs $ score
  
  print (textParts . o1 . objScr $ score)
--mapM_ (print . snt) (parts . o1 . objScr $ score)
  
  
  print (textParts . o2 . objScr $ score)
--mapM_ (print . snt) (parts . o2 . objScr $ score)
  
  putStrLn "\n"

-- | Print an argument score.
printArgScore :: ArgumentScore -> IO ()
printArgScore score = do
  print $ argScrOp score
  print $ rel score
  print $ argScrVal score
  let objMap = objRefMap score
  printArgCluster objMap $ a1 score
  printArgCluster objMap $ a2 score

-- | Print an argument cluster.
printArgCluster :: (ArgumentCluster -> IntMap Incidence) -> ArgumentCluster 
                -> IO ()
printArgCluster f a@ArgumentCluster {} = do
  mapM_ print . map (first unpack) . HashMap.toList $ relMap a
  mapM_ print . IntMap.toList $ f a
printArgCluster f a@D2ArgumentCluster {} = do
  mapM_ print . map (first unpack) . HashMap.toList . relMap $ acFst a
  mapM_ print . map (first unpack) . HashMap.toList . relMap $ acSnd a
  mapM_ print . IntMap.toList $ f a

