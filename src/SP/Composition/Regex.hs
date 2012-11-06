-- | Module for to be used for word compound detection.
module SP.Composition.Regex (mkRegexCompounds) where

import Data.Array (elems)
import Data.ByteString.Char8 (ByteString, concat, intercalate, pack)
import Data.Char (chr)
import Data.Either (rights)
import Data.Either.Utils (forceEither)
import Data.HashMap.Lazy (HashMap, fromList, lookupDefault, member)
import Data.List (nub, sortBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import Text.Regex (mkRegex, splitRegex)
import Text.Regex.TDFA (blankCompOpt, blankExecOpt)
import Text.Regex.TDFA.ByteString (compile, execute, Regex)
import SP.Boot.Sentence (toSentences)
import SP.Cluster
import SP.Config
import SP.Composition.Common
import SP.Execution.Update

-- | Convert a sentence to a ByteString of 1-char POS tag representations.
sentenceToPosStream :: HashMap ByteString ByteString -> [ObjCluster] -> ByteString
sentenceToPosStream posMap = Data.ByteString.Char8.concat . map (posToChar posMap)

-- | Convert a single part object cluster to 1-char POS tag representation.
posToChar :: HashMap ByteString ByteString -> ObjCluster -> ByteString
posToChar posMap = flip (lookupDefault errPos) posMap . pos . head . parts

-- | Generate a POS tag to 1-char ByteString map.
genPosMap :: [ObjCluster] -> HashMap ByteString ByteString
genPosMap = fromList . flip zip indices . nub . map (pos . head . parts)
  where
    indices = map (pack . replicate 1 . chr) ([65 .. 90] ++ [97 .. 121])

-- | A POS not found in articles, would be used only for very few articles.
errPos :: ByteString
errPos = pack [chr 122]

-- | Read in compound tuples from the compound file, two be used to build regex compositions.
readCompoundTuples :: [ByteString] -> HashMap ByteString ByteString -> IO [(ByteString, Regex, Int)]
readCompoundTuples posStream posMap = do
    config <- getConfig
    file <- readFile $ compoundFile config
    return . filterTuples posStream $ mapMaybe (lineToTuple posMap) (lines file)

-- | Only use tuples that use a minimum number of matches.
filterTuples :: [ByteString] -> [(ByteString, Regex, Int)] -> [(ByteString, Regex, Int)]
filterTuples posStreams = filter (\(_,regex,minUse) -> numMatches regex >= minUse) 
  where
    numMatches regex = length . catMaybes . rights . map (execute regex) $ posStreams

-- | Convert a line from the compound file to a compound tuple.    
lineToTuple :: HashMap ByteString ByteString -> String -> Maybe (ByteString, Regex, Int)
lineToTuple posMap line = if null commentNoComment || length postSplit < 3 || null tokens || not tokensOk
                          then Nothing 
                          else Just (pack posStr, toRegex pattern, read frqStr) 
  where
    -- The part of the line that isn't a comment.
    commentNoComment = splitRegex (mkRegex "#") line
    -- Match strings representing the new pos tag, the min. frequency and the pattern.
    postSplit = splitRegex (mkRegex "\\s*:\\s*") (head commentNoComment)
    [posStr, frqStr, patternStr] = postSplit
    -- Pattern tokens
    tokens = splitRegex (mkRegex "\\s+") patternStr
    pattern = Data.ByteString.Char8.concat . map toPatternChar $ tokens
    -- Check that tokens are okey
    tokensOk = all (\s -> length s <= 1 || member (pack s) posMap) tokens
    toRegex = forceEither . compile blankCompOpt blankExecOpt
    toPatternChar s = if length s <= 1
                      then pack s -- Assume 1 character string => Metacharacter
                      else lookupDefault errPos (pack s) posMap -- Assume longer => POS

-- | Slice an array, given offset and length.
slice :: (Int, Int) -> [a] -> [a]                                        
slice (from, len) = take len . drop from
{-# INLINE slice #-}

-- | Get the largest match from a set of compound tuples.
tupleMatch :: [(ByteString, Regex, Int)]        -- ^ Compound tuples. 
           -> ([ObjCluster], ByteString)        -- ^ A sentence with its POS stream representation. 
           -> [([ObjCluster], ByteString)]      -- ^ An object cluster compound set with a POS tag. 
tupleMatch tuples (snt,stream) = compoundPairs
  where
    toEither (_,regex,_) = execute regex stream
    matchPairs = zip tuples (catMaybes . rights . map toEither $ tuples)
    -- Find the largest match.
    expandMatchPair (tuple,match) = map (\e -> (tuple, e)) (elems match)
    matchElems = sortBy (comparing (negate . snd . snd)) 
               $ concatMap expandMatchPair matchPairs
    toCompoundPair ((newPos,_,_),range) = (slice range snt, newPos)
    -- Predicate function for only allowing clusters with "other" ner tags.
    allow (xs,_) = length xs > 1 && all ((== pack "O") . (ner . head . parts)) xs
    -- Create compound pairs.
    compoundPairs = filter allow $ map toCompoundPair (bestMatchElems matchElems [])

-- | Match elems.
bestMatchElems :: [((ByteString, Regex, Int), (Int, Int))] 
               -> [((ByteString, Regex, Int), (Int, Int))]
               -> [((ByteString, Regex, Int), (Int, Int))]
bestMatchElems []     ys = ys               
bestMatchElems (x:xs) ys = if all (independent (snd x) . snd) ys 
                           then bestMatchElems xs (x:ys)
                           else bestMatchElems xs ys

-- | True, iff the two supplied MatchArray elements are independent.
independent :: (Int, Int) -> (Int, Int) -> Bool
independent (x0,m) (y0,n) = x0 < y0 && x0 + m < y0 || y0 < x0 && y0 + n < x0

-- | Create regular expression compounds.
mkRegexCompounds :: [ObjCluster] -> IO Update
mkRegexCompounds objClusters = do
    -- Create pairs of sentences and their streams.
    let posMap = genPosMap objClusters
        sentences = toSentences objClusters
        sntStreamPairs = map (\snt -> (snt, sentenceToPosStream posMap snt)) sentences
    -- Compound tuples.
    tuples <- readCompoundTuples (map snd sntStreamPairs) posMap
    -- Match results and respective updates.
    let slicePosPairs = concatMap (tupleMatch tuples) sntStreamPairs
        updates = map mkPosComposition slicePosPairs
    -- Fold the updates and return.
    return $! foldUpdates updates

-- | Make a compound part.
mkPosCompoundPart :: [Part] -> ByteString -> Part
mkPosCompoundPart compParts compPos = (head compParts)
    { lemma = join ":" lemma
    , pos = compPos
    , ner = pack "O"
    , word = join " " word
    }
  where 
    join str prop = intercalate (pack str) (map prop compParts)
    
-- | Make a pos composition.
mkPosComposition :: ([ObjCluster],ByteString) -> Update
mkPosComposition (objClusters, compPos) = emptyUpdate
    {newObjClusters = [mkCompoundObjCluster compPart objClusters]}
  where
    compPart = mkPosCompoundPart (concatMap parts objClusters) compPos