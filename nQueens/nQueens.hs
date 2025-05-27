import System.Environment
import Data.List (permutations)
import Control.Monad (guard)

type Board = [Int]

nQueens :: Int -> Board -> Board
nQueens s b@(x:xs) | s == length xs  = xs
                   | s == x = []
                   | checkBoard x 1 xs && not (null r) = r
                   | otherwise = nQueens s ((x + 1):xs)
                   where r = nQueens s (0:b)

                         checkBoard :: Int -> Int -> Board -> Bool
                         checkBoard _ _ [] = True
                         checkBoard n offset (x:xs) = x /= n
                                                   && x /= n - offset
                                                   && x /= n + offset
                                                   && checkBoard n (offset + 1) xs

showQueens :: Int -> Board -> String
showQueens _ [] = ""
showQueens size (x:xs) = (concat . replicate x) "- "
                      ++ "Q "
                      ++ (concat . replicate (size - x - 1)) "- "
                      ++ "\n"
                      ++ showQueens size xs

main = do
    args <- getArgs
    guard . not . null $ args
    let size = read (head args) :: Int
    putStr . showQueens size . reverse . nQueens size $ [0] 

