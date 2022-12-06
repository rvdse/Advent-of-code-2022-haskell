-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.List

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show (maximum list) ++ "\n"
           ++ "Problem 2: " ++ show (sum $ take 3 (reverse $ sort list)) ++ "\n"
    where list = makeList (lines input)

makeList :: [String] -> [Int]
makeList [] = []
makeList elves = sum (map read firstElf):makeList restOfTheElves
    where firstElf = takeWhile (/="") elves
          restOfTheElves = dropWhile (=="") (dropWhile(/="") elves)
