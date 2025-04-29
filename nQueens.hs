import System.Environment
import Data.List (permutations)

nQueens :: Int -> [[Int]]
nQueens n = filter checkBoard . permutations . take n $ [0..]
    where checkBoard :: [Int] -> Bool
          checkBoard [_] = True
          checkBoard (x:xs) = checkRow x 1 xs && checkBoard xs
          
          checkRow :: Int -> Int -> [Int] -> Bool
          checkRow _ _ [] = True
          checkRow n offset (x:xs) = x /= n - offset
                                  && x /= n + offset
                                  && checkRow n (offset + 1) xs

showQueens :: Int -> [Int] -> String
showQueens _ [] = ""
showQueens size (x:xs) = replicate x '-'
                      ++ "Q"
                      ++ replicate (size - x - 1) '-'
                      ++ "\n"
                      ++ showQueens size xs

main = do
    args <- getArgs
    let size = read (head args) :: Int
    mapM_ (putStrLn . showQueens size) . nQueens $ size
