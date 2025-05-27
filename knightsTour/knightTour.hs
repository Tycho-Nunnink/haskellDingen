import System.Environment (getArgs)
import Data.List (intercalate)
type Pos = (Int, Int)
type Tour = [Pos]

knightsTour :: Int -> [Tour]
knightsTour n = foldl (>>=) (return [(1, 1)])
              $ replicate (n * n - 1)
              $ moveKnight n
    where moveKnight :: Int -> Tour -> [Tour]
          moveKnight n path@((x, y):xs) =
              [newPos:path | newPos@(x', y') <- [(x+1, y+2), (x+1, y-2), (x-1, y+2), (x-1, y-2),
                                                 (x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1)],
                             x' `elem` [1..n]
                          && y' `elem` [1..n]
                          && newPos `notElem` xs] -- één regel korter, joepie!!!!!!

              -- do newPos@(a', b') <- [(a+1, b+2), (a+1, b-2), (a-1, b+2), (a-1, b-2),
              --                        (a+2, b+1), (a+2, b-1), (a-2, b+1), (a-2, b-1)]
              --    guard (a' `elem` [1..n] -- "import Control.Monad (guard)" hiervoor nodig
              --        && b' `elem` [1..n]
              --        && newPos `notElem` xs)
              --    return (newPos:board) -- oud, had besloten het toch met een list comprehension te doen omdat het letterlijk hetzelfde doet en meer leesbaar is

showTour :: Tour -> String
showTour path = intercalate " -> " . map show $ reverse path

main = do
    (n:_) <- getArgs
    mapM (putStrLn . showTour) . knightsTour $ read n 
