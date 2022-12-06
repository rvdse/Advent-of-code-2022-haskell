-- The program expects the input on stdin, ie
-- $ ./solve < input

import Data.List (elemIndex)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show score1 ++ "\n"
           ++ "Problem 2: " ++ show score2 ++ "\n"
    where score1 = foldr score 0 (map shapes (lines input))
          score2 = foldr score 0 (map shapes2 (lines input))

score :: (Int, Int) -> Int -> Int
score (oppShape, myShape) acc = acc + myShape + gameScore oppShape myShape
    where gameScore opp me = 3*mod (1+mod (3+me-opp) 3) 3

shapes :: String -> (Int, Int)
shapes s = (shape $ head s, shape $ last s)

shapes2 :: String -> (Int, Int)
shapes2 s = (shape $ head s, 1 + mod (shape (head s) + shape (last s)) 3)

shape :: Char -> Int
shape c = case elemIndex c "ABCXYZ" of
    Just i -> 1 + mod i 3
    _ -> 0
