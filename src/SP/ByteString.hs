module SP.ByteString where

import qualified Data.ByteString.Char8 as B
import Data.Function
import Data.Hashable
import Data.Ord (comparing)

data ByteString = ByteString {bsStr :: B.ByteString, bsHsh :: !Int}
                  deriving (Read,Show)

instance Ord ByteString where
  compare = comparing bsStr

instance Eq ByteString where
  x == y = bsHsh x == bsHsh y && bsStr x == bsStr y

instance Hashable ByteString where
  hash = bsHsh

{-instance Show ByteString where 
  show = show.bsStr-}

pack s = ByteString b (hash b) where b = B.pack s
unpack = B.unpack . bsStr
concat bs = ByteString b (hash b) where b = B.concat $ map bsStr bs
seize n = B.take n . bsStr 
