-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.List (elemIndex)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show p1 ++ "\n"
           ++ "Problem 2: " ++ show p2 ++ "\n"
    where p1 = foldr ((+).score) 0 (map (head.dupes.halves) (lines input))
          p2 = foldr ((+).score) 0 (findBadges (lines input))

-- Find common character from first 3 lines, continue recursively.
findBadges :: [String] -> String
findBadges (e:f:g:es) = head (dupes (e,(dupes (f,g)))):findBadges es
findBadges _ = ""

-- Find the common characters in two strings.
dupes :: (String, String) -> String
dupes (c1, c2) = filter ((flip elem) c1) c2

-- Split a string in half.
halves :: String -> (String, String)
halves s = splitAt (div (length s) 2) s

-- Score of a character from the problem description.
score :: Char -> Int
score c = case elemIndex c (['a'..'z'] ++ ['A'..'Z']) of
    Just i -> i + 1
    _ -> 0
