halve :: [x] -> ([x], [x])
halve x = splitAt (length x `div` 2) x

merge :: (Ord x) => [x] -> [x] -> [x]
merge a [] = a
merge [] b = b
merge a@(aH:aT) b@(bH:bT)
    | aH < bH = aH:merge aT b
    | otherwise = bH:merge a bT

sort :: (Ord x) => [x] -> [x]
sort [x] = [x]
sort x = merge (sort a) (sort b) where (a,b) = halve x



