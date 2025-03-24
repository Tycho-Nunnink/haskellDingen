polish :: String -> Float
polish = head . foldl func [] . words
    where func :: [Float] -> String -> [Float]
          func (b:a:xs) "+" = (a + b) : xs
          func (b:a:xs) "-" = (a - b) : xs
          func (b:a:xs) "*" = (a * b) : xs
          func (b:a:xs) "/" = (a / b) : xs
          func xs num | all (`elem` ['0'..'9']) num = read num : xs
          func xs _ = xs
