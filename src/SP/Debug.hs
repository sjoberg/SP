module SP.Debug where

import Debug.Trace

-- | Tracing that shows the value.
vtrace :: (Show a) => String -> a -> a
vtrace s v = trace (s ++ ": " ++ show v) v

-- | A, on the output value, conditional trace.
ctrace :: (Show a) => (a -> Bool) -> String -> a -> a
ctrace f s v = if f v then vtrace s v else v

-- | A, on the output value, conditional trace.
cerror :: (Show a) => (a -> Bool) -> String -> a -> a
cerror f s v = if f v then error s else v

