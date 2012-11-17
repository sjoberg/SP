module Main where

import Control.Monad (liftM)
import System.Exit (exitSuccess)
import System.CPUTime (getCPUTime)
import SP.Cluster
import SP.Config
import SP.Boot.MongoDB
import SP.Boot.Preprocess (lemmaGroups, removeEmpty)
import SP.Boot.SchmittTrigger
import SP.Boot.Sentence
import SP.Composition.Ner (mkNerCompounds)
import SP.Composition.Regex (mkRegexCompounds)
import SP.Reduction.Reducer (reduce, updateDomain)
import SP.Reduction.Update (newObjClusters)
import SP.Scoring.Score
import SP.Scoring.Scorer

main :: IO ()
main = bootstrap Main.init

init :: [ObjCluster] -> IO ()
init clusters = do
    -- Configuration.
    cfg <- getConfig
    -- Trim & compose NER compounds.
    let clusters1 = updateDomain (mkNerCompounds $ trim clusters) clusters
    -- Compose regex compounds.
    clusters2 <- liftM (flip updateDomain clusters1) (mkRegexCompounds clusters1)
    -- Remove empty clusters.
    let clusters3 = removeEmpty clusters2
    -- Phase i.
    putStrLn "Starting phase i."
    lemmaIterTrigger >>= phasei cfg clusters3
    exitSuccess

-- | Cluster by lemma within the same POS category.
phasei :: Config -> [ObjCluster] -> Trigger -> IO [ObjCluster]
phasei cfg clusters trigger = do
    startTime <- getCPUTime
    putStrLn "Här:"
    --print . length . pair $ clusters
    putStrLn "SLut-"
    --mapM_ (print . word . head . parts . objLeft) scores
    if lowFlankPulse trigger trigger'
        then do
            putStrLn "Phase i done."
            return $! clusters
        else do
            -- Print elapsed time.
            print . length $ scores
            exitSuccess
            --liftM ((/1000) . (0- startTime)) getCPUTime >>= print
            phasei cfg clusters' trigger'
  where
    groups = lemmaGroups clusters
    scores = alignBest $ mkScores cfg groups
    maxPoints = if null scores then 0 else scoreValue $ head scores
    trigger' = updateState trigger maxPoints
    clusters' = reduce clusters scores
    
