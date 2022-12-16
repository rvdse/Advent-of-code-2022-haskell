-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.Char (ord)
import Data.List (elemIndex, findIndex)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show p1 ++ "\n"
           ++ "Problem 2: " ++ show p2 ++ "\n"
    where p1 = pos (lines input) 'S'
          p2 = pos (lines input) 'E'
          e = elevation input

-- Find the position of an element in a 2D list.
pos :: [[a]] -> a -> (Int, Int)
pos haystack needle = case findIndex (elem needle) haystack of
        Just y -> case elemIndex needle y of
            Just x -> (x, y)
            _ -> (0, 0)
        _ -> (0, 0)

-- = case elemIndex c ys of
--        Just y -> (y, x-1)
--        _ -> (0, 0)
--    where (x, ys) = head (filter (\(i, t) -> elem c t) (zip [1..] (lines s)))

--posHelper :: (Int, Int) -> String -> Char -> (Int, Int)

elevation :: String -> Int -> Int -> Int
elevation s x y
    | x < 0 || y < 0 || x >= maxx || y >= maxy = 9999
    | char == 'S' = 1
    | char == 'E' = 26
    | elem char ['a'..'z'] = ord char - 96
    | otherwise = 9999
    where maxx = length (takeWhile (/='\n') s)
          maxy = length (lines s)
          char = head (drop x (head (drop y (lines s))))
-- graph :: String -> 
