-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.List (foldl')

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show p1 ++ "\n"
           ++ "Problem 2:\n"
           ++ show (drawLine (take 40 regVals)) ++ "\n"
           ++ show (drawLine (take 40 (drop 40 regVals))) ++ "\n"
           ++ show (drawLine (take 40 (drop 80 regVals))) ++ "\n"
           ++ show (drawLine (take 40 (drop 120 regVals))) ++ "\n"
           ++ show (drawLine (take 40 (drop 160 regVals))) ++ "\n"
           ++ show (drawLine (take 40 (drop 200 regVals))) ++ "\n"
    where p1 = sum $ map (sigStr regVals) [20, 60, 100, 140, 180, 220]
          regVals = reverse (foldl' f [1] instructions)
          instructions = map readInstruction (lines input)

data Instruction = NOOP | ADDX Int deriving (Show)

readInstruction :: String -> Instruction
readInstruction ('a':'d':'d':'x':' ':s) = ADDX (read s)
readInstruction _ = NOOP

f :: [Int] -> Instruction -> [Int]
f (v:vs) (ADDX x) = v+x:v:v:vs
f (v:vs) NOOP = v:v:vs
f v _ = v

sigStr :: [Int] -> Int -> Int
sigStr vs i = i * (last (take i vs))

drawLine :: [Int] -> String
drawLine r = drawLineHelper 0 r

drawLineHelper :: Int -> [Int] -> String
drawLineHelper _ [] = []
drawLineHelper i (r:rs)
    | abs (r-i) <= 1 = '#':drawLineHelper (i+1) rs
    | otherwise = '.':drawLineHelper (i+1) rs

