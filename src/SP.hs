
import Control.Monad.Trans (liftIO)
import SP.Bootstrapper
import SP.Cluster
import SP.Score
import System.Environment

main :: IO ()
main = bootstrap cluster 

cluster arts = liftIO $ print $ bestScores $ objClustersFromArts $ take 10 arts

