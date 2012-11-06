module Main where

import SP.Cluster
import SP.Boot.MongoDB
import SP.Boot.Preprocess (removeEmpty)
import SP.Composition.Ner (mkNerCompounds)
import SP.Composition.Regex (mkRegexCompounds)
import SP.Execution.Redirect
import SP.Execution.Update
import System.Exit (exitSuccess)
import Control.Monad (liftM)
import Data.List (nub)

main :: IO ()
main = bootstrap Main.init

init :: [ObjCluster] -> IO ()
init clusters = do
    -- Compose regex compounds.
    clusters' <- liftM (redirect clusters) (mkRegexCompounds clusters)
    -- Compose NER compounds.
    let clusters'' = redirect clusters' (mkNerCompounds clusters')
    exitSuccess
