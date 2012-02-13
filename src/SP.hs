import Control.Monad.Trans (liftIO)
import Data.List
import Data.Ord
import SP.ByteString
import SP.Cluster
import SP.Config
import SP.Bootstrap.MongoDB
import SP.Preprocess.Compound
import SP.Preprocess.Preprocess
import SP.Score.Score
import SP.Score.Scorer
import Text.Printf (printf)
import Prelude
import System.Exit

main :: IO ()
main = bootstrap start

start :: [Partition] -> IO ()
start ptns1 = liftIO $ do
  config <- getConfig
  let ptns2 = takePartitions config ptns1
      ptns3 = mergeNerCompounds ptns2
  iter 1 ptns3
  exitSuccess

iter :: Int -> [Partition] -> IO ()
iter n ptns = liftIO $ do
  printf "Starting iteration %d.\n" n
  let groups = groupByPos ptns
      best = maxOperatorScores 0.25 groups
--print ptns
  mapM_ (putStrLn.show) best
  {-nptns = merge best ptns
  if null best 
    then return nptns
    else return (iter (n+1) nptns)-}
--mapM_ ((putStrLn . unwords . map (unpack.text.head.parts)).sortBy (comparing ocId).ocs) ptns 
