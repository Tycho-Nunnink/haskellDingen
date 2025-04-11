eerste :: String -> String
eerste [] = []
eerste [letter] = [letter]
eerste woord@(letter:_) = "de eerste letter van het woord " ++ woord ++ " is " ++ [letter]

eerste' :: String -> String
eerste' woord | length woord <= 1 = woord
              | otherwise = "de eerste letter van het woord " ++ woord ++ " is " ++ [letter]
                  where letter = head woord -- (letter:_) = woord
