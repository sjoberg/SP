{-# LANGUAGE BangPatterns #-}

-- | Math module.
module SP.Math where

import Data.List (foldl')
import Data.Word (Word16)

-- | Mean by function. For lists smaller than or equal to 65536.
meanBy :: (a -> Double) -> [a] -> Double
meanBy f xs | null xs    = 0
            | otherwise = total / fromIntegral size
  where
    total   :: Double
    size    :: Word16
    (total,size) = foldl' (\(!s, !l) x -> (s + f x, l + 1) ) (0,0) xs

-- | Mean. For lists smaller than or equal to 65536.
mean :: [Double] -> Double
mean = meanBy id

-- | Harmonic mean of two doubles.
hmean2 :: Double -> Double -> Double
hmean2 x y | x == 0 && y == 0 = 0
           | otherwise = x * y * 2 / (x + y)

-- | Harmonic mean of three doubles.
hmean3 :: Double -> Double -> Double -> Double
hmean3 x y z | x == 0 && y == 0 && z == 0 = 0
             | otherwise = x * y * z * 3 / (x * y + x * z + y * z)

-- | Exponential prior. I hope.
prior :: Double -> Double -> Double
prior 0 x = x
prior p x = (exp ((-p) * x) - 1) / (exp (-p) - 1)