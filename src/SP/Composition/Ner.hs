-- | Module for NER composition.
module SP.Composition.Ner (mkNerCompounds) where

import Data.ByteString.Char8 (intercalate, pack)
import Data.Function (on)
import Data.List (groupBy)
import SP.Cluster
import SP.Boot.Sentence (toSentences)
import SP.Composition.Common (mkCompoundObjCluster)
import SP.Reduction.Update

-- | Make NER compounds.
mkNerCompounds :: [ObjCluster] -> Update
mkNerCompounds = foldUpdates . map toUpdate . groups

-- | Make an update out of a group
toUpdate :: [ObjCluster] -> Update
toUpdate g = let objCluster = mkCompoundObjCluster (mkNerCompoundPart g) g
             in emptyUpdate {objTuples = map (flip (,) objCluster) g}

-- | Group by NER tag, for all sentences.
groups ::  [ObjCluster] -> [[ObjCluster]]
groups = filter ((> 1) .length) .  filter isntMisc . concatMap groupByNer . toSentences

-- | Group by NER tag, for one sentence.
groupByNer :: [ObjCluster] -> [[ObjCluster]]
groupByNer = groupBy ((==) `on` (ner . head . parts))

-- | True if a group doesn't have the misc NER tag.
isntMisc :: [ObjCluster] -> Bool
isntMisc = (/= pack "O") . ner . head . parts . head

-- | Make a compound part.
mkNerCompoundPart :: [ObjCluster] -> Part
mkNerCompoundPart objClusters = (head compParts)
    { lemma = join ":" lemma
    , pos = (ner . last) compParts
    , word = join " " word
    }
  where 
    compParts = map (head . parts) objClusters -- Compound parts
    join str prop = intercalate (pack str) (map prop compParts)