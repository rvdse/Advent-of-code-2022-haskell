-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.Char (ord)
import Data.List (transpose)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ (show.numVisible) trees ++ "\n"
           ++ "Problem 2: " ++ (show.maximum.scenicScore) trees ++ "\n"
    where trees = map (map (\c->ord c-48)) (lines input) :: [[Int]]

-- Calculate score for each tree, but return as a flat list.
scenicScore :: [[Int]] -> [Int]
scenicScore t = zipWith (*) viewHorz viewVert
    where rowScore t = zipWith (*) (view t) (reverse.view.reverse $ t)
          viewHorz = concat $ map rowScore t
          viewVert = concat $ transpose (map rowScore (transpose t))

-- View from each element in list towards the end.
view :: [Int] -> [Int]
view (t:[]) = [0]
view (t:ts) = minimum [length ts, 1+length (takeWhile (<t) ts)] : view ts

-- Count the number of visible trees.
numVisible :: [[Int]] -> Int
numVisible t = length $ filter id (zipWith (||) visHorz visVert)
    where visHorz = concat $ map visibility t
          visVert = concat $ transpose (map visibility (transpose t))

-- Visibility of each element in list from either end.
visibility :: [Int] -> [Bool]
visibility t = zipWith (||) (visFromEnd t) (visFromOtherEnd t)
    where visFromEnd (t:[]) = [True]
          visFromEnd (t:ts) = (t > maximum ts) : visFromEnd ts
          visFromOtherEnd = reverse.visFromEnd.reverse

