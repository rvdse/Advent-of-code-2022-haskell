-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.Set (fromList, size)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show (findUniq input 4) ++ "\n"
           ++ "Problem 2: " ++ show (findUniq input 14) ++ "\n"

-- Find the index of the first unique substring
-- of a particular length n.
findUniq :: String -> Int -> Int
findUniq s n = n + findHelper s n 0

-- Helper function to keep track of index while searching.
findHelper :: String -> Int -> Int -> Int
findHelper s len i
    | i > length s = 0 -- Base case, failed to find.
    | size (fromList (take len (drop i s))) == len = i
    | otherwise = findHelper s len (i+1)
