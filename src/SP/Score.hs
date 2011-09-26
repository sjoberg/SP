module SP.Score where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List
import Data.List.Extras (argmax, argmaxes)
import Data.Ord (comparing)
import SP.Cluster

-- | A generic score.
data Scr = Scr {objScr::ObjScr, argScrs::[ArgScr], u1,u2::[ArgClr]} 
  deriving (Show, Eq) 

-- | An operator score.
data OpScr = MrgScr {val::Double, scr::Scr}
           | AbsScr {val::Double, scr::Scr}
           | PrnResScr {val::Double, scr::Scr}
           | AddChdScr {val::Double, scr::Scr, parOc,chdOc::ObjClr}
           deriving (Show, Eq)

-- | Object score.
data ObjScr = ObjScr {objScrVal::Double, oc1, oc2::ObjClr} 
  deriving (Show, Eq)

-- | Argument score.
data ArgScr = ArgScr {argScrVal::Double, ac1, ac2::ArgClr} 
  deriving (Show, Eq)

sOc1 = oc1 . objScr
sOc2 = oc2 . objScr

-- | Calculates the similarity between two argument clusters.
cmpac :: ArgClr -> ArgClr -> Double
cmpac (AdjArgClr _ _ xs _) (AdjArgClr _ _ ys _) = cmpAdjArgs xs ys
cmpac (RmtArgClr _ _ xs _) (RmtArgClr _ _ ys _) = cmpRmtArgs xs ys
cmpac _                     _                   = 0

-- | Calculates the similarity between two lists of adjacent arguments.
cmpAdjArgs :: [AdjArg] -> [AdjArg] -> Double
cmpAdjArgs xs ys = 0.5 * cmpFld rel xs ys + 0.5 * cmpFld obj xs ys

-- | Calculates the similarity between two lists of remote arguments.
cmpRmtArgs :: [RmtArg] -> [RmtArg] -> Double
cmpRmtArgs xs ys = scr intRel + scr rmtRel + scr intObj + scr rmtObj 
  where scr fld = 0.25 * cmpFld fld xs ys

-- | Calculates the similarity between two dependencies with respect to a 
-- | field.
cmpFld :: (Eq b) => (a -> b) -> [a] -> [a] -> Double
cmpFld f xs ys = 
  if null uniques 
    then 0.0 
    else sum [1.0 - abs (freq f v xs - freq f v ys) | v <- uniques] 
           / genericLength uniques
  where uniques = unique f xs ys

-- | The frequency of the value v in the dependencies xs.
freq :: (Eq b) => (a -> b) -> b -> [a] -> Double
freq f v xs = genericLength (filter (\x -> f x == v) xs) / genericLength xs

-- | The unique values of the property, transformed by f, in the dependencies 
-- in xs, ys.
unique :: (Eq b) => (a -> b) -> [a] -> [a] -> [b]
unique f xs ys = nub (map f xs `intersect` map f ys)

-- | Get the best combination of argument scores. Greedy search.
bestArgScrs :: ObjClr -> ObjClr -> [ArgScr]
bestArgScrs oc1 oc2 = takeBest ac1 ac2 $ sortBy (key argScrVal) allScrs
  where
    allScrs = scrFor parArgClrs ++ scrFor chdArgClrs ++ scrFor sblArgClrs 
    scrFor fld = allArgScrs (fld oc1) (fld oc2)

-- | Get the best combination of scores among the supplied scores.
-- Input must be sorted descending by score value.
takeBest :: (Eq s, Eq c) => (s -> c) -> (s -> c) -> [s] -> [s]
takeBest _  _  []     = []
takeBest f1 f2 (s:ss) = s : takeBest f1 f2 (filter (indep f1 f2 s) ss)

-- | Returns true if the specified scores are independent.
-- f1 and f2 are accessor methods for the clusters of the scores.
indep :: (Eq s, Eq c) => (s -> c) -> (s -> c) -> s -> s -> Bool
indep f1 f2 s1 s2 = null $ intersect [f1 s1, f2 s1] [f1 s2, f2 s2]

-- | Key for descending sort of scores.
key :: (a -> Double) -> a -> a -> Ordering
key f s1 s2 = comparing negate (f s1) (f s2)

-- | Get all argument scores.
allArgScrs :: [ArgClr] -> [ArgClr] -> [ArgScr]
allArgScrs xs ys = zipWith3 ArgScr (zipWith cmpac xs ys) xs ys

-- | Create an operator score for two object clusters.
scrObjClr :: ObjClr -> ObjClr -> OpScr
scrObjClr c1 c2 = toOpScr $ Scr os ass (fuacss c1) (fuacss c2)
  where
    ass = bestArgScrs c1 c2
    os = ObjScr (cmpFld form (ocParts c1) (ocParts c2)) c1 c2
    -- Filter Unmerged Argument Cluster ScoreS.
    fuacss c = filter (`notElem` uacss) $ ocArgClrs c
    uacss = concatMap (\as -> [ac1 as, ac2 as]) ass

scrVal :: Scr -> (Double, Double)
scrVal s = (objScrVal $ objScr s, if argScrSize == 0 then 0 else avgArgScr)
  where avgArgScr = sum (map argScrVal $ argScrs s) / argScrSize
        argScrSize = genericLength $ argScrs s

-- | Calculate the best scores such that their object clusters are independent.
bestScrs :: [ObjClr] -> [OpScr]
bestScrs xs = takeBest (oc1 . objScr . scr) (oc2 . objScr . scr) $ 
                filter (\s -> val s > 0.0) $ 
                  argmaxes val (allScrs xs)
    
-- | Calculate all scores. Parallel.
allScrs :: [ObjClr] -> [OpScr]
allScrs xs = parMap rpar f [(x,y) | x <- xs, y <- xs, x /= y]
  where f = uncurry scrObjClr

-- | Converts a generic score to an operator score.
toOpScr :: Scr -> OpScr
toOpScr s = argmax val opScrs
  where
    -- Score value tuple.
    sv = scrVal s
    -- Object Score Value.
    osv = fst sv
    -- Argument Score Value.
    asv = snd sv
    -- Length of cluster groups, and their sum.
    tot = x + y + z
    x = gl argScrs; y = gl u1; z = gl u2; gl f = genericLength $ f s
    -- Score for abstracting: 1 - the weighted coefficient of variation.
    -- No abs needed for mean in the coefficient of variation, since mean 
    -- currently always is positive.
    absScrVal = (1.0 - sqrt varW / mean) * asv / 2.0
      where
        mean = tot / 3.0
        varW = ((x - mean)^2 + (y - mean)^2 + (z - mean)^2) / 6.0
    -- Score for merging.
    -- The share of merged arg. clusters.
    mrgScrVal = (osv + x / tot * asv) / 2.0
    -- Create add child scores.
    dcv = cv x y - cv x z
      where
        -- Adjusted coefficient of variation for two samples.
        cv x1 x2 = sqrt( ( (x1 - mean)^2 + (x2 - mean)^2) / 6.0 ) / mean
          where mean = (x1 + x2) / 2.0
    -- List of add child scores.
    addChdScrs = 
      [AddChdScr ((1 - abs dcv) * asv / 2.0) s oc1 oc2 | y>x && x>z] ++
      [AddChdScr ((1 - abs dcv) * asv / 2.0) s oc2 oc1 | z>x && x>y] ++
      [AddChdScr (dcv * asv / 2.0)           s oc1 oc2 | dcv > 0 && y>x] ++
      [AddChdScr (dcv * asv / 2.0)           s oc2 oc1 | dcv > 0 && x>y] ++
      [AddChdScr ((-dcv) * asv / 2.0)        s oc1 oc2 | dcv < 0 && z<x] ++
      [AddChdScr ((-dcv) * asv / 2.0)        s oc2 oc1 | dcv < 0 && x<z]
      where oc1 = sOc1 s; oc2 = sOc2 s
    -- Operator score candidates.
    opScrs = MrgScr mrgScrVal s : AbsScr absScrVal s : addChdScrs

