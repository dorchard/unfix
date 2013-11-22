{-# LANGUAGE TemplateHaskell #-}

import Unfix 

unfix [d| fib :: Int -> Int
          fib 0 = 1
          fib 1 = 1
          fib n = fib (n - 1) + fib (n - 2) |]