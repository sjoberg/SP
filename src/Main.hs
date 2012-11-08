module Main where

import SP.Cluster
import SP.Config
import SP.Boot.MongoDB
import SP.Boot.Preprocess (lemmaGroups, removeEmpty)
import SP.Boot.SchmittTrigger
import SP.Composition.Ner (mkNerCompounds)
import SP.Composition.Regex (mkRegexCompounds)
import SP.Execution.Executor
import SP.Execution.Redirect
import SP.Scoring.Score
import SP.Scoring.Scorer
import System.Exit (exitSuccess)
import System.CPUTime (getCPUTime)
import Control.Monad (liftM)

main :: IO ()
main = bootstrap Main.init

init :: [ObjCluster] -> IO ()
init clusters = do
    -- Configuration.
    cfg <- getConfig
    -- Compose regex compounds.
    clusters1 <- liftM (redirect clusters) (mkRegexCompounds clusters)
    -- Compose NER compounds.
    let clusters2 = redirect clusters1 (mkNerCompounds clusters1)
    -- Remove empty clusters.
    let clusters3 = removeEmpty clusters2 
    -- Phase i.
    lemmaIterTrigger >>= phasei cfg clusters3
    exitSuccess
    
phasei :: Config -> [ObjCluster] -> Trigger -> IO [ObjCluster]
phasei cfg clusters trigger = do
    startTime <- getCPUTime
    if lowFlankPulse trigger trigger'
        then 
            return $! clusters
        else do
            liftM (flip (-) startTime) getCPUTime >>= print
            phasei cfg clusters' trigger'
  where
    groups = lemmaGroups clusters
    scores = alignBest (mkScores cfg groups)
    maxPoints = if null scores then 0 else scoreValue $ head scores
    trigger' = updateState trigger maxPoints
    clusters' = execute clusters scores
