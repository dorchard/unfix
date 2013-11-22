{-# LANGUAGE TemplateHaskell #-}

import Unfix 
import Data.Function.Memoize

-- fib :: (Int -> Int) -> Int -> Int (new type after unfix)
unfix [d| fib' :: Int -> Int
          fib' 0 = 1
          fib' 1 = 1
          fib' n = fib' (n - 1) + fib' (n - 2) |]

fib = memoFix fib'