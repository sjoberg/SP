module SP.Serialization where

import SP.Cluster
import System.IO

store :: FilePath -> [Partition] -> IO ()
store filePath partitions = writeFile filePath (show partitions)

load :: FilePath -> IO [Partition]
load filePath = do
  stream <- readFile filePath
  return $ read stream
