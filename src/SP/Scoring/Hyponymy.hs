
module SP.Scoring.Hyponymy where

import Data.HashMap.Lazy (difference, elems, intersectionWith)
import SP.Cluster
import SP.Config
import SP.Scoring.Argument (filterMergers, meanError)
import SP.Math (hmean2, mean, prior)
import SP.Scoring.Score
import SP.Scoring.UnqualifiedSet (truncIntersectionErrs)
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.HashSet (HashSet)

-- | Score candidates for each operator.
data CandidateSet = CandidateSet {merge, abstract, parent, child :: ArgScore}

-- | Calculate the frequency sum for argument clusters in an object cluster.
frequencySum :: ObjCluster -> Double
frequencySum = sum . map frequency . allArgClusters

-- | Convert score to a score for the merge, abstract, parent, or child operator.
hyponymize :: Config -> HashSet Int -> Score -> Score
hyponymize cfg unqlfSet score@Score {objLeft = x, objRight = y, argScores = mergers} = score 
    { scoreOp = operator
    , scoreValue = points
    , argScores = scoreSet
    }
  where
    (abstractScores, parentScores, childScores) = groupSelects cfg . map (argHyponymize cfg unqlfSet) $ mergers
    scoreSets = map (toScoreSet (frequencySum x) (frequencySum y)) 
        [ (Merge, filterMergers cfg mergers)
        , (Abstract, catMaybes abstractScores)
        , (Parent, catMaybes parentScores)
        , (Child, catMaybes childScores)
        ]
    -- Maximum by comparing scores points.
    ((operator,points),scoreSet) = maximumBy (comparing $ snd . fst) scoreSets

-- | Calculate score totals for a score set (operator and scores tuples).
toScoreSet :: Double -> Double -> (Operator,[ArgScore]) -> ((Operator,Double),[ArgScore])
toScoreSet fsumx fsumy (op,scores) = if all ((/= op) . argScoreOp) scores 
                                     then ((op, 0), scores) 
                                     else ((op, sum $ map weigh scores), scores)
  where
    weigh score = let sx = argScoreValue score * (frequency . argLeft) score / fsumx
                      sy = argScoreValue score * (frequency . argRight) score / fsumy
                  in case op of
        Parent -> sy
        Child -> sx
        _ -> hmean2 sx sy 

-- | Extract scores from all candidate sets.
groupSelects :: Config -> [CandidateSet] -> ([Maybe ArgScore],[Maybe ArgScore],[Maybe ArgScore])
groupSelects cfg = unzip3 . map (\set ->
    let choose ftx fop = select (tm cfg) (ftx cfg) (merge set) (fop set)
    in (choose ta abstract, choose tc parent, choose tc child))

-- | Choose between two scores.
select :: Double -> Double -> ArgScore -> ArgScore -> Maybe ArgScore
select tx ty x y | sy > sx && sy >= ty = Just y
                 | sy > sx && sx >= tx = Just x
                 | sy <= sx && sx >= tx = Just x
                 | sy <= sx && sy >= ty = Just y
                 | otherwise = Nothing
  where
    sx = argScoreValue x -- Score value for merge operator.
    sy = argScoreValue y -- Score value for other operator.

-- | Points excluding end object points.
errExclEnd :: ArgCluster -> ArgCluster -> Double
errExclEnd x y | nearestNeighbour x = meanError relFrequency x y
               | otherwise = let p = meanError relFrequency x y / 3
                                 q = meanError objFrequency x y / 3
                                 r = meanError subRelFrequency x y / 3
                             in hmean2 p q + r

-- | Create argument score for other operators than merge.
argHyponymize :: Config -> HashSet Int -> ArgScore -> CandidateSet
argHyponymize cfg unqlfSet score@ArgScore {argLeft = x, argRight = y} = CandidateSet
    { merge = score
    , abstract = score {argScoreOp = Abstract, argScoreValue = points wa xyTruncPoints}
    , parent = score {argScoreOp = Parent, argScoreValue = points wc (xyErrors ++ yErrors)}
    , child = score {argScoreOp = Child, argScoreValue = points wc (xyErrors ++ xErrors)}
    }
  where 
    -- Maps to end object cluster ids.
    ex = endObjMap x
    ey = endObjMap y
    
    -- Errors
    xyErrors = elems (intersectionWith (\p q -> abs (p - q)) ex ey)
    xErrors = elems (difference ex ey)
    yErrors = elems (difference ey ex)
    xyTruncPoints = map (1 -) (truncIntersectionErrs unqlfSet x y)
    
    -- Calculate points for operators.
    points f = prior (f cfg) . (1 -) . (/ 2) . (+ errExclEnd x y) . mean
