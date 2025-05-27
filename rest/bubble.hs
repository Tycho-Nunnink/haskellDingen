module Bubble (bubble) where

parse :: (Ord a) => [a] -> [a]
parse [x] = [x]
parse (y:x:xs)
    | y > x = x:parse (y:xs)
    | otherwise = y:parse (x:xs)

bubble :: (Ord a) => [a] -> [a]
bubble [x] = [x]
bubble x = bubble (init parsed) ++ [last parsed] 
    where parsed = parse x
