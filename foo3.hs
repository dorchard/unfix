{-# LANGUAGE TemplateHaskell #-}

import Unfix 
import Data.Function.Memoize

memo [d|
 fib' :: Int -> Int
 fib' 0 = 1
 fib' 1 = 1
 fib' n = fib' (n - 1) + fib' (n - 2) |]
