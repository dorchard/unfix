{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.Unfix 

import Data.Function.Memoize

-- Fibonacci example with type signature
-- New type after unfix is: fib :: (Int -> Int) -> Int -> Int

unfix 
 [d| fib' :: Int -> Int
     fib' 0 = 1
     fib' 1 = 1
     fib' n = fib' (n - 1) + fib' (n - 2) |]

fib = memoFix fib'

-- Fibonacci with no type signature
-- Inferred type after unfix is: fib' :: (Eq a, Num a, Num b) => (a -> b) -> (a -> b)

unfix 
 [d| fib'' 0 = 1
     fib'' 1 = 1
     fib'' n = fib'' (n - 1) + fib'' (n - 2) |]

-- This example has a more general type
unfix 
 [d| fibB :: (Eq a, Num a) => (a -> a) 
     fibB 0 = 1
     fibB 1 = 1
     fibB n = fibB (n - 1) + fibB (n - 2) |]

-- Use "refix" to call "memoFix" on the unfixed function

refix [| memoFix |]
          [d| fibA :: Int -> Int 
              fibA 0 = 1
              fibA 1 = 1
              fibA n = fibA (n - 1) + fibA (n - 2) |]

-- *Main> :t fibA
-- fibA :: Int -> Int
-- *Main> fibA 100
-- 1298777728820984005
