module Fac (fac) where
import Data.Function (fix)
fac :: Int -> Int
fac = product . enumFromTo 1

fac' :: Int -> Int
fac' n = product [1..n]

fac'' :: Int -> Int
fac'' 0 = 1
fac'' n = n * fac (n-1)

fac''' :: Int -> Int
fac''' = fix (\recc n -> if n == 0 then 1 else n * recc (n-1)) --inline reccursion????
