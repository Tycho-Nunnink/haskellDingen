module Average (average) where
average a b | a /= 0 && b /= 0 = (a + b)/2
            |otherwise = 0
