{-# OPTIONS_GHC -XBangPatterns #-}
module SP.Score.Math where

import Data.Word (Word16,Word32)
import Data.List.Stream
import Prelude hiding (null)

amean2, hmean2, ca2 :: Double -> Double -> Double
amean2 x y = (x + y) / 2
hmean2 x y = x * y * 2 / (x + y)
ca2 x y = 1 / m * sqrt(0.5 * ((x-m)^2 + (y-m)^2))
  where m = amean2 x y

amean3, ca3 :: Double -> Double -> Double -> Double
amean3 x y z = (x + y + z) / 3
ca3 x y z = 1 / m * sqrt (1 / 6 * ((x-m)^2 + (y-m)^2 + (z-m)^2))
  where m = amean3 x y z

ca4 :: Double -> Double -> Double -> Double -> Double
ca4 x y z w = 1 / m * sqrt (1 / 12 * ((x-m)^2 + (y-m)^2 + (z-m)^2 + (w-m)^2))
  where m = (x + y + z + w) / 4

cardinality :: [a] -> Double
cardinality xs = length' xs 0
  where length' :: [a] -> Word16 -> Double
        length' []     n = fromIntegral n
        length' (x:xs) n = length' xs $! (n + 1)

cardinality32 :: [a] -> Double
cardinality32 xs = length' xs 0
  where length' :: [a] -> Word32 -> Double
        length' []     n = fromIntegral n
        length' (x:xs) n = length' xs $! (n + 1)

meanBy :: (a -> Double) -> [a] -> Double
meanBy f l | null l    = 0
           | otherwise = sum / fromIntegral len 
  where sum :: Double
        len :: Word16
        (sum,len) = foldl' (\(!s, !l) e -> (s + (f e), l + 1) ) (0,0) l

mean :: [Double] -> Double
mean = meanBy id

--mean :: [Double] -> Double 
--mean l | null l    = 0
--       | otherwise = sum / fromIntegral len
--  where sum :: Double 
--        len :: Word16
--        (sum,len) = foldl' (\(!s, !l) e -> (s + e, l + 1) ) (0,0) l
