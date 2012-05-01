import Control.Arrow
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
import SP.Print
import SP.Score.Score
import SP.Score.Scorer
import Text.Printf (printf)
import System.Exit
import Data.Maybe (fromJust)

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)

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
  initial <- iter 1 snt groupByForm (Low 0.9 0.65) 1 False preprocessed
  printf "Done.\n"

  printf "Clustering...\n"
  completed <- iter 3 snt groupByPos (Low 0.9 0.6) 1 True initial
  printf "Done.\n"

  exitSuccess

iter :: Int -> (Part -> String) -> ([Partition] -> [[ObjectCluster]]) 
     -> SchmittTrigger -> Int -> Bool -> [Partition] -> IO [Partition]
iter sampleLimit snt groupFcn trigger n isa partitions = liftIO $ do
  printf "Starting iteration %d.\n" n

  let 
      removeEmpty :: [[ObjectCluster]] -> [[ObjectCluster]]
      removeEmpty = map (filter $ \o -> length (parts o) >= sampleLimit)
      
      groups :: [[ObjectCluster]]
      groups = removeEmpty . filterPronouns . groupFcn $ partitions

      -- Best scores.
      bestScores = maxOperatorScores isa groups

      -- New partitions.
      newPartitions = mergePartitionsSimple bestScores partitions

      -- Process value.
      pv = opScrVal . head $ bestScores
  
      -- Function for printing scores and continue iterating.
      doNextRound trigger = do 
        mapM_ (printScore snt) bestScores
        iter sampleLimit snt groupFcn trigger (n + 1) isa newPartitions

  -- Enter next iteration or stop iterating.
  case trigger of
    Low th tl  -> doNextRound (if pv >= th then High th tl else trigger)
    High th tl -> if pv >= tl then doNextRound trigger
                              else return partitions

filterPronouns = filter allow
  where allow o = let headPartPos = pos.head.parts.head $ o
                  in headPartPos `notElem` map pack ["PR", "CC", "WP", "DT"]

data SchmittTrigger = High Double Double | Low Double Double

