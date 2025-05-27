module Prod (prod) where
    prod :: Num a => [a] -> a
    prod = foldl (*) 1
