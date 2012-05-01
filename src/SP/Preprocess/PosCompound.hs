module SP.Preprocess.PosCompound where

import Control.Arrow
import Data.Array (elems)
import qualified Data.ByteString.Char8 as B
import Data.Either.Utils (forceEither)
import Data.Function (on)
import Data.HashMap.Strict (fromList)
import Data.List (groupBy)
import SP.ByteString (bsStr, pack, ByteString)
import SP.Cluster
import SP.Preprocess.Compound
import SP.Redirect
import Text.Regex.Base.RegexLike (MatchArray)
import Text.Regex.TDFA.ByteString
import Text.Regex.TDFA.Common hiding (on)

-- | Replacements.
replacements = [ ("VBZ VBN TO", "1")
               , ("VBN IN", "VBN")
               ]

createPosCompounds :: [Partition] -> [Partition]
createPosCompounds = map $ \p -> redirect p $ fromList . objList $ p

-- | Tuple list for object clusters.
objList :: Partition -> [(ObjectCluster,ObjectCluster)]
objList partition = let sentences = partitionSentences partition
                        sntPosPair = map $ \s -> (createPosString s,s)
                        regexReplacements = compileReplacements replacements
                    in concat [ concatMap (groupToList r) slices
                              | (regex,r) <- regexReplacements
                              , (p,s) <- sntPosPair sentences
                              , let slices = toSlices regex p s]

-- | Create list for hash maps.
groupToList :: ByteString -> [ObjectCluster] -> [(ObjectCluster,ObjectCluster)]
groupToList pos group = zip group (repeat $ setPos pos $ merge group)
                    
-- | Set the POS tag of an object cluster with only one part.
setPos :: ByteString -> ObjectCluster -> ObjectCluster
setPos pos o@(ObjectCluster {parts = [part]}) = o {parts = [part {pos = pos}]}

-- | Slices a sentence using a regex.
toSlices :: Regex -> B.ByteString -> [ObjectCluster] -> [[ObjectCluster]]
toSlices regex b xs = map (sliceMatch b xs) (matchList $ execute regex b)

-- | Match array to a list of matches.
matchList :: Either String (Maybe MatchArray) -> [(Int,Int)]
matchList m = case forceEither m of Just ma -> elems ma
                                    Nothing -> []

-- | Compilation options for regular expressions.
compOption :: CompOption
compOption = CompOption { caseSensitive  = True
                        , multiline      = False
                        , rightAssoc     = True
                        , newSyntax      = False
                        , lastStarGreedy = False
                        }

-- | Execution option for a regular expression.
execOption :: ExecOption
execOption = ExecOption { captureGroups = True }

compileReplacements :: [(String,String)] -> [(Regex, ByteString)]
compileReplacements = map convert . packReplacements
  where convert = first $ forceEither . compile compOption execOption

-- | Pack a list of replacement pairs.
packReplacements :: [(String,String)] -> [(B.ByteString, ByteString)]
packReplacements = map (B.pack *** pack)

-- | Slice object clusters using a match.
sliceMatch :: B.ByteString -> [ObjectCluster] -> (Int,Int) -> [ObjectCluster]
sliceMatch b xs match = slice (second (+1) (convPos b *>> match)) xs

-- | Slice a list.
slice :: (Int, Int) -> [a] -> [a]                                               
slice (from, to) = take to . drop from

-- | Convert position of a match.
convPos :: B.ByteString -> Int -> Int
convPos b i = B.count ' ' $ B.take i b

-- | Creates a string of POS tags appearing in a sentence.
createPosString :: [ObjectCluster] -> B.ByteString
createPosString = B.intercalate (B.pack " ") . map (bsStr . pos . head . parts)

-- | Splits up a partition into sentences.
partitionSentences :: Partition -> [[ObjectCluster]]
partitionSentences = groupBy ((==) `on` sntId . head . parts) . ocs

-- | Applies f to the elements of a pair.
(*>>) f = f *** f
