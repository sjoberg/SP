
module Tests.AllTests where

import Control.Arrow ((***))
import Data.ByteString.Char8 (pack)
import Data.HashSet (empty)
import Data.List (sortBy)
import Data.Ord (comparing)
import Test.HUnit
import Text.Printf (printf)
import SP.Boot.Preprocess (lemmaGroups, posCatGroups)
import SP.Cluster
import SP.Reduction.Reducer (reduce)
import SP.Reduction.Merger (merge)
import SP.Reduction.Update
import SP.Scoring.Score
import SP.Scoring.Scorer
import Constructors

snt1, snt2 :: [ObjCluster]
c1, c2, c3, c4, c5, c6 :: ObjCluster
a1, a2, a3, a4, a5, a6, a7, a8 :: ArgCluster

-- | Test sentence 1.
snt1 = [c1, c2, c3]
c1 = newObjCluster 1 "Partake" "partake" "VB"  [] [a1] []
c2 = newObjCluster 2 "your" "you" "PRP$" [a4] [] []
c3 = newObjCluster 3 "soup" "soup" "NN" [a2] [a3] []
-- | Argument clusters.
(a1,a2) = newParChdPair 1 2 3 1 "dobj"
(a3,a4) = newParChdPair 3 4 2 3 "poss"

-- | Test sentence 2.
snt2 = [c4, c5, c6]
c4 = newObjCluster 4 "Eat" "eat" "VB" [] [a5] []
c5 = newObjCluster 5 "your" "you" "PRP$" [a8] [] []
c6 = newObjCluster 6 "soup" "soup" "NN" [a6] [a7] []
-- | Argument clusters.
(a5,a6) = newParChdPair 5 6 6 4 "dobj"
(a7,a8) = newParChdPair 7 8 5 6 "poss"

-- | Compare scores between two cluster with an expected score value (from a
-- corresponding matrix element).
elTest :: [ObjCluster]  -- ^ The object clusters.
       -> Int           -- ^ Matrix row.
       -> Int           -- ^ Matrix column.
       -> Double        -- ^ Expected value.
       -> Test          -- ^ Resulting test.
elTest cs i j e = let s = score defCfg empty (cs !! i, cs !! j) 
                  in TestLabel ("Element " ++ show (i,j)) (valTest s e)

-- | Test all scores by comparing scores against the matrix values.
matTest :: [[Double]]   -- ^ Matrix of expected score values.
        -> [ObjCluster] -- ^ Object clusters.
        -> [Test]       -- ^ Resulting test.
matTest ss cs = concat $ zipWith (\r ri -> zipWith (elTest cs ri) [0..] r) ss [0..]

-- | Test score value.
valTest :: Score -> Double -> Test
valTest s e = TestCase (assertEqual "Test score value" e $ scoreValue s)

-- | Test length of a list.
lengthTest :: [a] -> Int -> Test
lengthTest li e = TestCase (assertEqual "Test length" e $ length li)

-- | Full domain, iteration 1.
dom :: [ObjCluster]
dom = snt1 ++ snt2

-- | Matrix of expected score values, iteration 1.
mat :: [[Double]]
mat = [[1.0, 0.0, 0.0, 0.5, 0.0, 0.0],
       [0.0, 1.0, 0.0, 0.0, 0.5, 0.0],
       [0.0, 0.0, 1.0, 0.0, 0.0, 0.5],
       [0.5, 0.0, 0.0, 1.0, 0.0, 0.0],
       [0.0, 0.5, 0.0, 0.0, 1.0, 0.0],
       [0.0, 0.0, 0.5, 0.0, 0.0, 1.0]]

scores, bestScores :: [Score]
-- | All scores of iteration 1, when no grouping has been used.
scores = mkScores defCfg [dom]
-- | Best scores with grouping by lemma.
bestScores = alignBest . mkScores defCfg $ lemmaGroups dom

-- | Iteration 1 tests.
it1Tests :: [Test]
it1Tests = lengthTest bestScores 2
         : lengthTest scores (sum [1..5]) -- Num. el:s above diagonal (or below). 
         : matTest mat dom -- Test whole matrix.

-- | Iteration 2.
dom' :: [ObjCluster]
dom' = sortBy (comparing objClusterId) $ reduce dom bestScores

mat' :: [[Double]]
mat' = [[1.0, 0.0, 0.0, 1.0],
        [0.0, 1.0, 0.0, 0.0],
        [0.0, 0.0, 1.0, 0.0],
        [1.0, 0.0, 0.0, 1.0]]

scores', bestScores' :: [Score]
scores' = mkScores defCfg [dom']
bestScores' = alignBest . mkScores defCfg $ posCatGroups dom'

-- | Iteration 2 tests.
it2Tests :: [Test]
it2Tests = lengthTest bestScores' 1
         : valTest (head bestScores') 1
         : lengthTest scores' (sum [1..3]) -- Num. el:s above diagonal (or below). 
         : matTest mat' dom' -- Test whole matrix.

-- | Iteration 3.
dom'' :: [ObjCluster]
dom'' = sortBy (comparing objClusterId) $ reduce dom' bestScores'

mat'' :: [[Double]]
mat'' = [[1.0, 0.0, 0.0],
         [0.0, 1.0, 0.0],
         [0.0, 0.0, 1.0]]

scores'', bestScores'' :: [Score]
scores'' = mkScores defCfg [dom'']
bestScores'' = alignBest . mkScores defCfg $ [dom'']

-- | Iteration 2 tests.
it3Tests :: [Test]
it3Tests = lengthTest bestScores'' 3
         : TestCase (assertEqual "Test length" [0,0,0] $ map scoreValue scores'')
         : matTest mat'' dom'' -- Test whole matrix.

-- | All tests.
tests :: Test
tests = (TestList . concat) [it1Tests, it2Tests, it3Tests]

-- Main.
main :: IO ()
main = do
    runTestTT tests >>= print
    return ()

