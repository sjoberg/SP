
import Control.Monad.Trans (liftIO)
import Data.List (genericLength)
import Data.List.Extras
import SP.Bootstrapper
import SP.Cluster
import SP.Score
import SP.Merge
import System.CPUTime
import System.Environment
import Text.Printf

main :: IO ()
main = bootstrap cluster

cluster :: [Art] -> [ObjClr] -> IO ()
cluster arts objClrs = liftIO $ do
  print arts
  iter 1 objClrs

iter :: Int -> [ObjClr] -> IO () -- [ObjClr]
iter n cs = liftIO $ do
  printf "%d object clusters.\n" $ length cs
  printf "Making iteration %d...\n" n
  start <- getCPUTime
  let ss = bestScores cs -- Scores.
  printf "Total score: %.2f\n" $ sum $ map scoreVal ss
  printf "Number of scores: %d\n" $ length ss
  printf "Score: %.2f\n" $ scoreVal $ head ss
  let result = mergeClusters cs ss
  end <- getCPUTime
  let elapsed :: Double
      elapsed = fromIntegral (end - start) / 10^9
  printf "Time elapsed: %.2f s\n" $ elapsed / 10^3
  printf "Time elapsed / cluster: %.2f ms\n" $ elapsed / genericLength cs
  print $ argmaxes (length . ocParts) result
  return ()
  {-if scoreVal (head ss) >= 0.25 
    then iter (n+1) result
    else return ()-}

