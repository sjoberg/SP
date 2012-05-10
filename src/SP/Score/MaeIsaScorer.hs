-- Mean absolute error isa scorer.
module SP.Score.MaeIsaScorer where

import Debug.Trace
import Control.Arrow (second)
import Data.IntMap (difference, elems, intersectionWith)

import SP.ByteString
import SP.Cluster
import SP.Debug
import SP.Score.Argument
import SP.Score.Math (argmax, mean)
import SP.Score.Score

-- | Transform an operator score to an operator score with ISA support.
isaOperatorScore :: ParamSet -> OperatorScore -> OperatorScore
isaOperatorScore paramSet@ParamSet {wm = wm, wa = wa, wc = wc} operatorScore = 
  let -- Merge and ISA argument scores
      mrgArgScrs = traceShow (toParts o1, toParts o2) $ argScrs operatorScore
      isaArgScrs = concatMap (isaArgumentScores paramSet) mrgArgScrs

      toParts f = map (unpack . text) . parts . f . objScr $ operatorScore

      -- Incidence sums.
      iSum1 = incidenceSum . o1 . objScr $ operatorScore
      iSum2 = incidenceSum . o2 . objScr $ operatorScore

      -- Tuples with a weighted mark and the scores, for each set of scores
      mark w op = let scrs = filter ((== op) . argScrOp) isaArgScrs
                  in (scrs, (w *) . vtrace (show op) . matchMark iSum1 iSum2 $ scrs)
      mrgMark = (mrgArgScrs, vtrace "Merge" $ wm * opScrVal operatorScore)
      absMark = mark wa Abstract
      parMark = mark wc Parent
      chdMark = mark wc Child

      -- Elected scores, mark, and operator
      (elScrs,elMark) = argmax snd [mrgMark, absMark, chdMark, parMark]
      elOp = argScrOp $ head elScrs
  in operatorScore {op = elOp, argScrs = elScrs, opScrVal = elMark}

-- | Transform an argument score to an argument score with ISA support.
isaArgumentScores :: ParamSet -> ArgumentScore -> [ArgumentScore]
isaArgumentScores ParamSet {ta = ta, tc = tc} score = 
  let relMark = scoreRelation (a1 score) (a2 score)

      -- Maps of object cluster references
      m = objRefMap score $ a1 score
      n = objRefMap score $ a2 score

      -- Errors
      absErr x y   = abs $ x - y
      matchErrs    = elems $ intersectionWith absErr m n
      noMatchErrs1 = elems $ difference m n
      noMatchErrs2 = elems $ difference n m

      -- Marks

      -- Transform errors to marks
      mark errs = (1 - mean errs + relMark) / 2
      -- Arg. cluster 1 is parent to arg. cluster 2
      parMark = mark $ matchErrs ++ noMatchErrs1
      -- Arg. cluster 2 is parent to arg. cluster 1
      chdMark = mark $ matchErrs ++ noMatchErrs2
      -- Arg. clusters 1 and 2 have a common cluster
      absMark = mark matchErrs

  in [score {argScrVal = parMark, argScrOp = Parent}   | parMark >= tc]
  ++ [score {argScrVal = chdMark, argScrOp = Child}    | chdMark >= tc]
  ++ [score {argScrVal = absMark, argScrOp = Abstract} | absMark >= ta]

