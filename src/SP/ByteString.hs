module SP.ByteString where

import qualified Data.ByteString.Char8 as B
import Data.Hashable
import Data.Ord (comparing)
import Data.List.Stream
import Prelude hiding (map)

data ByteString = ByteString {bsStr :: B.ByteString, bsHsh :: !Int}
                  deriving (Read,Show)

instance Ord ByteString where
  compare = comparing bsStr

instance Eq ByteString where
  x == y = bsHsh x == bsHsh y

instance Hashable ByteString where
  hash = bsHsh

{-instance Show ByteString where 
  show = show.bsStr-}

pack s = ByteString b (hash b) where b = B.pack s
unpack bs = B.unpack $ bsStr bs
concat bs = ByteString b (hash b) where b = B.concat $ map bsStr bs
