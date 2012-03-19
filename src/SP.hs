import Control.Monad.Trans (liftIO)
import Data.Function
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

import Data.List.Stream
import Prelude hiding ( (!!), filter, head, length, map, notElem, unwords
                      , zipWith
                      )

main :: IO ()
main = bootstrap start

start :: [Partition] -> IO ()
start partitions = liftIO $ do
  config <- getConfig

  let cleaned = cleanArticles partitions
      merged = mergeCompounds . createRegexCompounds $ cleaned
      preprocessed = filterEmpty merged
      arts = toArticles merged
      snt :: Part -> String
      snt part = (arts !! artId part) !! sntId part

  printf "Creating compounds...\n"
  
  printf "Removing %d articles.\n" $ length partitions - length cleaned

  mapM_ print $ toSntTuples partitions

--printText $ merged
--print $ length $ Prelude.concat arts
--print $ length . nub . Prelude.concat $ arts
--putStrLn $ intercalate "\n\n" . Prelude.concat $ arts

--exitSuccess

  printf "Merging by form...\n"
  initial <- iter 0 snt groupByForm (Low 0.9 0.55) 1 preprocessed
  printf "Done.\n"

  printf "Clustering...\n"
  completed <- iter 2 snt groupByPos (Low 0.9 0.6) 1 initial
  printf "Done.\n"

  exitSuccess

iter :: Int -> (Part -> String) -> ([Partition] -> [[ObjectCluster]]) 
     -> SchmittTrigger -> Int -> [Partition] -> IO [Partition]
iter sampleLimit snt groupFcn trigger n partitions = liftIO $ do
  printf "Starting iteration %d.\n" n

  let 
      removeEmpty :: [[ObjectCluster]] -> [[ObjectCluster]]
      removeEmpty = map (filter $ \o -> length (parts o) > sampleLimit)
      
      groups :: [[ObjectCluster]]
      groups = removeEmpty . filterPronouns . groupFcn $ partitions

      -- Best scores.
      bestScores = maxOperatorScores 0.5 groups

      -- New partitions.
      newPartitions = mergePartitionsSimple (map objScr bestScores) partitions

      -- Process value.
      pv = opScrVal . head $ bestScores
  
      -- Function for printing scores and continue iterating.
      doNextRound trigger = do mapM_ (printScore snt) bestScores
                               iter sampleLimit snt groupFcn trigger (n + 1) newPartitions

  -- Enter next iteration or stop iterating.
  case trigger of
    Low th tl  -> doNextRound (if pv >= th then High th tl else trigger)
    High th tl -> if pv >= tl then doNextRound trigger
                              else return partitions

filterPronouns = filter allow
  where allow o = let headPartPos = pos.head.parts.head $ o
                  in headPartPos `notElem` map pack ["PR", "CC", "WP", "WP$"]

printText :: [Partition] -> IO ()
printText = mapM_ $ putStrLn . unwords . toText . sort . ocs
  where toText = map $ unpack . text . head . parts
        sort = sortBy (comparing ocId)

toArticles :: [Partition] -> [[String]]
toArticles = map toSentences

toSntTuples :: [Partition] -> [((Int,Int),String)]
toSntTuples = let toTuple artId    = zipWith (\sId s -> ((artId,sId),s)) [0..]
                  toArts artId ptn = toTuple artId $ toSentences ptn
              in Prelude.concat . zipWith toArts [0..]

toSentences :: Partition -> [String]
toSentences = let sort = sortBy $ comparing ocId
                  toText = unwords . map (unpack . text . head . parts)
                  sntEq = (==) `on` (sntId . head . parts)
              in map toText . groupBy sntEq . sort . ocs

textParts :: ObjectCluster -> [String]
textParts oc = map display $ parts oc

display part = intercalate "-" $ map ($ part) [ unpack . form
                                              , show . artId
                                              , show . sntId ]

printScore :: (Part -> String) ->  OperatorScore -> IO ()
printScore snt score = do
  print (opScrVal score)
--mapM_ print $ objArgScrs . objScr $ score
  print (textParts . o1 . objScr $ score)
--mapM_ (print . snt) (parts . o1 . objScr $ score)
  print (textParts . o2 . objScr $ score)
--mapM_ (print . snt) (parts . o2 . objScr $ score)
  putStrLn "\n"

data SchmittTrigger = High Double Double | Low Double Double
