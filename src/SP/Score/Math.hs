{-# LANGUAGE BangPatterns #-}
module SP.Score.Math where

import Data.List (foldl1', foldl')
import Data.Word (Word16,Word32)

amean2, hmean2, ca2 :: Double -> Double -> Double

-- | Arithmetic mean. Two parameter.
amean2 x y = (x + y) / 2

-- | Harmonic mean. Two parameters.
hmean2 x y | x == 0 && y == 0 = 0
           | otherwise        = x * y * 2 / (x + y)

-- | Coefficient of variation. Three parameters.
ca2 x y | m == 0    = 0
        | otherwise = 1 / m * sqrt(0.5 * ((x-m)^2 + (y-m)^2))
  where m = amean2 x y

amean3, ca3 :: Double -> Double -> Double -> Double

-- | Arithmetic mean. Three parameters.
amean3 x y z = (x + y + z) / 3

-- | Adjusted coefficient of variation. Three parameters.
ca3 x y z = 1 / m * sqrt (1 / 6 * ((x-m)^2 + (y-m)^2 + (z-m)^2))
  where m = amean3 x y z

-- | Adjusted coefficient of variation. Four parameters.
ca4 :: Double -> Double -> Double -> Double -> Double
ca4 x y z w = 1 / m * sqrt (1 / 12 * ((x-m)^2 + (y-m)^2 + (z-m)^2 + (w-m)^2))
  where m = (x + y + z + w) / 4

-- | Faster length using word16 internally.
cardinality :: [a] -> Double
cardinality xs = length' xs 0
  where length' :: [a] -> Word16 -> Double
        length' []     n = fromIntegral n
        length' (x:xs) n = length' xs $! (n + 1)

-- | Faster length using Word32 internally.
cardinality32 :: [a] -> Double
cardinality32 xs = length' xs 0
  where length' :: [a] -> Word32 -> Double
        length' []     n = fromIntegral n
        length' (x:xs) n = length' xs $! (n + 1)

-- | Mean by.
meanBy :: (a -> Double) -> [a] -> Double
meanBy f l | null l    = 0
           | otherwise = sum / fromIntegral len 
  where sum :: Double
        len :: Word16
        (sum,len) = foldl' (\(!s, !l) e -> (s + f e, l + 1) ) (0,0) l

-- | Mean.
mean :: [Double] -> Double
mean = meanBy id

-- | Sum by.
sumBy :: (a -> Double) -> [a] -> Double
sumBy f = foldl' (\t e -> t + f e) 0

-- | Argument maximum with strict fold and stream fusion.
-- Does not cache result of f.
argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f = foldl1' $ \x y -> if f x >= f y then x else y

