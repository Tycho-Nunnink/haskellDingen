import System.Environment (getArgs)
import Data.List (intercalate)
type Pos = (Int, Int)
type Tour = [Pos]

knightsTour :: Int -> [Tour]
knightsTour n = foldl (>>=) (return [(1, 1)])
               $ replicate (n * n - 1)
               $ moveKnight n
    where moveKnight :: Int -> Tour -> [Tour]
          moveKnight n path@((a, b):xs) =
              [newPos:path | newPos@(a', b') <- [(a+1, b+2), (a+1, b-2), (a-1, b+2), (a-1, b-2),
                                                 (a+2, b+1), (a+2, b-1), (a-2, b+1), (a-2, b-1)],
                             a' `elem` [1..n]
                          && b' `elem` [1..n]
                          && newPos `notElem` xs] -- één regel korter, joepie!!!!!!
              -- do newPos@(a', b') <- [(a+1, b+2), (a+1, b-2), (a-1, b+2), (a-1, b-2),
              --                        (a+2, b+1), (a+2, b-1), (a-2, b+1), (a-2, b-1)]
              --    guard (a' `elem` [1..n] -- "import Control.Monad (guard)" hiervoor nodig
              --        && b' `elem` [1..n]
              --        && newPos `notElem` xs)
              --    return (newPos:board) -- oud, had besloten het toch met een list comprehension te doen omdat het letterlijk hetzelfde is

showTour :: Tour -> String
showTour xs = intercalate " -> " . map show $ reverse xs

main = do
    (n:_) <- getArgs
    
    mapM (putStrLn . showTour) . knightsTour $ read n 
