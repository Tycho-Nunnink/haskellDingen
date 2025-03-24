findMax :: (Ord x) => [x] -> x
findMax [x] = x
findMax (x:xs)
    | x > xsMax = x
    | otherwise = xsMax
    where xsMax = findMax xs
