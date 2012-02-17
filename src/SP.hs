import Control.Monad.Trans (liftIO)
import Data.List
import Data.Ord
import SP.ByteString
import SP.Cluster
import SP.Config
import SP.Bootstrap.MongoDB
import SP.Merge
import SP.Preprocess.Compound
import SP.Preprocess.Preprocess
import SP.Score.Score
import SP.Score.Scorer
import Text.Printf (printf)
import Prelude 
import System.Exit

import Data.Maybe (fromJust)
import qualified Data.IntMap as IntMap

main :: IO ()
main = bootstrap start

start :: [Partition] -> IO ()
start partitions = liftIO $ do
  config <- getConfig

  let limit = takePartitions config
      preprocessed = mergeCompounds . limit $ partitions
  
  printf "Merging by form..."
  initial <- iter groupByForm 0.25 1 preprocessed
  printf "Done."

  printf "Clustering..."
  completed <- iter groupByPos 0.25 1 initial
  printf "Done."

  exitSuccess

iter :: ([Partition] -> [[ObjectCluster]]) -> Double -> Int -> [Partition] 
     -> IO [Partition]
iter groupFcn threshold n partitions = liftIO $ do
  printf "Starting iteration %d.\n" n

  let groups = filterPronouns . groupFcn $ partitions
      bestScores = maxOperatorScores 0.5 groups
      newPartitions = mergePartitionsSimple (map objScr bestScores) partitions
 
  if (opScrVal.head) bestScores > threshold
    then do mapM_ printScore bestScores
            iter groupFcn threshold (n + 1) newPartitions
    else return partitions 

filterPronouns = filter allow
  where allow o = let headPartPos = pos.head.parts.head $ o
                  in headPartPos `notElem` map pack ["PRP", "PRP$", "CC"]

printText :: [Partition] -> IO ()
printText = mapM_ $ putStrLn . unwords . toText . sort . ocs
  where toText = map $ unpack . text . head . parts
        sort = sortBy (comparing ocId)

printScore :: OperatorScore -> IO ()
printScore score = do
  print (opScrVal score)
  print (o1.objScr $ score)
  print (o2.objScr $ score)
  putStrLn "\n"

 
