module Rev (rev) where
    rev :: [a] -> [a]
    rev = foldl (flip (:)) []
