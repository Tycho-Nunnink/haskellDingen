import System.Environment

type Board = [Int]

nQueens :: Int -> [Board]
nQueens n = foldl (>>=) (return []) . replicate n $ addQueens n
          where addQueens :: Int -> Board -> [Board]
                addQueens n board = [x:board | x <- [0..n-1], checkBoard 1 x board]

                checkBoard :: Int -> Int -> Board -> Bool
                checkBoard _ _ [] = True
                checkBoard offset n (x:xs) = x /= n
                                          && x /= n - offset
                                          && x /= n + offset
                                          && checkBoard (offset + 1) n xs

showQueens :: Int -> Board -> String
showQueens _ [] = ""
showQueens size (x:xs) = (concat . replicate x) "- "
                      ++ "Q "
                      ++ (concat . replicate (size - x - 1)) "- "
                      ++ "\n"
                      ++ showQueens size xs

main = do
    (arg:_) <- getArgs
    let n = read arg :: Int
    mapM (putStrLn . showQueens n) . nQueens $ n

