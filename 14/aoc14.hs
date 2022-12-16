-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.Char (isDigit)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show p1 ++ "\n"
           ++ "Problem 2: " ++ show p2 ++ "\n"
    where p1 = rockLines
          p2 = 2
          rockLines =  (map (fill.rl) (lines input))

rl :: String -> [(Int, Int)]
rl [] = []
rl s = (read x, read y) : rl (dropWhile (not.isDigit) rest)
    where (x, temp) = break (==',') s
          (y, rest) = break (=='-') (tail temp)

fill :: [(Int, Int)] -> [(Int, Int)]
fill ((_,_):[]) = []
fill ((x1,y1):(x2,y2):ll) = zip (drop (minimum ([1..])) (cycle [x1]) ++ fill (x2,y2):ll

