
import Control.Monad
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
--print arts
  iter 1 objClrs

iter :: Int -> [ObjClr] -> IO ()
iter n clrs = liftIO $ do
  printf "Making iteration %d...\n" n
  printf "%d object clusters.\n" $ length clrs
  start <- getCPUTime
  -- Scores.
  let scores = bestScrs clrs
  printf "Total score: %.2f\n" $ sum $ map val scores
  printf "Number of scores: %d\n" $ length scores
  -- Merge clusters.
  let result = mergeClusters clrs scores
  -- End and print timing.
  end <- getCPUTime
  let elapsed :: Double
      elapsed = fromIntegral (end - start) / 10^9
  -- Elapsed time, seconds.
  printf "Time elapsed: %.2f s\n" $ elapsed / 10^3 
  -- Elapsed time / cluster, milliseconds.
  printf "Time elapsed / cluster: %.2f ms\n" $ elapsed / genericLength clrs
  -- Print and contingently continue.
  print scores
  unless (null scores) $ iter (n + 1) result

