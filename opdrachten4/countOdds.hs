module CountOdds (countOdds) where
    countOdds = length . filter odd
