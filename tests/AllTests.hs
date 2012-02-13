module Tests.AllTests where 

import Data.ByteString.Char8 (pack)
import Data.List
import Data.Maybe
import Test.HUnit
import SP.Cluster
import SP.Merge
import SP.Score

-- Object clusters, test sentence 1.
ocsSnt1 = [oc1, oc2, oc3]
  where
    oc1 = ObjClr {ocId=1, ocParts=[Part {pId=1, word=pack "Partake", 
                                         form=pack "partake:VB"}],
                  parArgClrs=[], chdArgClrs=[fst pc1], sblArgClrs=[]}
    oc2 = ObjClr {ocId=2, ocParts=[Part {pId=2, word=pack "your", 
                                         form=pack "you:PRP$"}],
                  parArgClrs=[snd pc2], chdArgClrs=[], sblArgClrs=[]}
    oc3 = ObjClr {ocId=3, ocParts=[Part {pId=3, word=pack "soup", 
                                         form=pack "soup:NN"}],
                  parArgClrs=[snd pc1], chdArgClrs=[fst pc2], sblArgClrs=[]}
    pc1 = mkPCPair 1 2 oc1 oc3 "dobj"
    pc2 = mkPCPair 3 4 oc3 oc2 "poss"

-- Object clusters, test sentence 2.
ocsSnt2 = [oc1, oc2, oc3]
  where
    oc1 = ObjClr {ocId=4, ocParts=[Part {pId=4, word=pack "Eat", 
                                         form=pack "eat:VB"}],
                  parArgClrs=[], chdArgClrs=[fst pc1], sblArgClrs=[]}
    oc2 = ObjClr {ocId=5, ocParts=[Part {pId=5, word=pack "your", 
                                         form=pack "you:PRP$"}],
                  parArgClrs=[snd pc2], chdArgClrs=[], sblArgClrs=[]}
    oc3 = ObjClr {ocId=6, ocParts=[Part {pId=6, word=pack "soup", 
                                         form=pack "soup:NN"}],
                  parArgClrs=[snd pc1], chdArgClrs=[fst pc2], sblArgClrs=[]}
    pc1 = mkPCPair 5 6 oc1 oc3 "dobj"
    pc2 = mkPCPair 7 8 oc3 oc2 "poss"

-- Create a parent / child arguments.
mkPCPair id1 id2 p c rel = (AdjArgClr {aacId=id1, aacObj=p, aacRole=Chd,
                                       aacArgs=[AdjArg (pack rel) c]},
                            AdjArgClr {aacId=id2, aacObj=c, aacRole=Par,
                                       aacArgs=[AdjArg (pack rel) p]})

-- Compare two object clusters against an expected value.
testCmp ocs oi1 oi2 e = TestLabel (show oi1 ++ " vs. " ++ show oi2) tc
  where
    tc = TestCase $ assertEqual "Compare" e (val score)
    score = scrObjClr (ocs !! oi1) (ocs !! oi2)

-- Compare object cluster merge scores against a matrix.
testCmps m ocs = 
  concat $ zipWith (\r ri -> zipWith (testCmp ocs ri) [0..] r) m [0..]

-- Test compare score values.
testScoreVal score e = TestCase $ assertEqual "Score val" e (val score)

-- Test length of a list.
testLen li e = TestCase $ assertEqual "Cmp len" e (length li)

tests = TestList $ iter1Tests ++ iter2Tests ++ iter3Tests ++ iter4Tests

-- Iteration 1 tests.
iter1Tests = scoreLenTest1:bestScores1Test1:bestScores1Test2:cmpTests1

-- Test score values.
ocs1 = ocsSnt1 ++ ocsSnt2
mat1 = [[1.0, 0.0, 0.0, 0.25, 0.0, 0.0],
        [0.0, 1.0, 0.0, 0.0, 0.75, 0.0],
        [0.0, 0.0, 1.0, 0.0, 0.0, 0.75],
        [0.25, 0.0, 0.0, 1.0, 0.0, 0.0],
        [0.0, 0.75, 0.0, 0.0, 1.0, 0.0],
        [0.0, 0.0, 0.75, 0.0, 0.0, 1.0]]
cmpTests1 = testCmps mat1 ocs1

-- Test best score length resulting from iteration 1.
scoreLenTest1 = testLen scores1 2
scores1 = bestScrs ocs1
bestScores1Test1 = testScoreVal (head scores1) 0.75
bestScores1Test2 = testScoreVal (last scores1) 0.75

-- Iteration 2 tests.
iter2Tests = ocs2LenTest:cmpTests2
ocs2 = mergeClusters ocs1 scores1
scores2 = bestScrs ocs2

-- Test merge length.
ocs2LenTest = testLen ocs2 4
-- Test score values.
mat2 = [[1.0, 0.5, 0.0, 0.0],
        [0.5, 1.0, 0.0, 0.0],
        [0.0, 0.0, 1.0, 0.0],
        [0.0, 0.0, 0.0, 1.0]]
cmpTests2 = testCmps mat2 ocs2

-- Iteration 3 tests.
iter3Tests = ocs3LenTest:cmpTests2
ocs3 = mergeClusters ocs2 scores2
scores3 = bestScrs ocs3

-- Test merge length.
ocs3LenTest = testLen ocs3 3
-- Test score values.
mat3 = [[1.0, 0.0, 0.0],
        [0.0, 1.0, 0.0],
        [0.0, 0.0, 1.0]]
cmpTests3 = testCmps mat3 ocs3

-- Iteration 4.
iter4Tests = [scoreLenTest4]
scoreLenTest4 = testLen scores3 0

-- Main.
main :: IO ()
main = do 
--print $ allScrs ocs1
  runTestTT tests
--putStrLn "Result: "
--print ocs3
  return ()

