{-# LANGUAGE TypeSynonymInstances #-}
module SP.DeepSeq where

import Control.DeepSeq
import qualified Data.ByteString.Internal as BS
import SP.ByteString
import SP.Cluster
import Data.HashMap.Lazy
import Data.IntMap

instance NFData Partition where
  rnf (Partition _ ocs acs) = rnf ocs `seq` rnf acs

instance NFData ObjectCluster where
  rnf (ObjectCluster _ parts pars chdn sbls) = (rnf parts `seq` rnf pars) `seq`
                                               (rnf chdn `seq` rnf sbls)

instance NFData ArgumentCluster where
  rnf (ArgumentCluster _ pm cm rm) = rnf rm -- `seq` (rnf pm `seq` rnf cm)
  rnf (D2ArgumentCluster x y) = rnf x `seq` rnf y

instance NFData Part where
  rnf Part {form = form, lemma = lemma, ner = ner, pos = pos} = 
    rnf form `seq` (rnf lemma `seq` (rnf ner `seq` rnf pos))

instance NFData ByteString where
  rnf (ByteString _ h) = rnf h

