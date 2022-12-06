-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.Char (isDigit, isUpper)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show (map head finalCratesPt1) ++ "\n"
           ++ "Problem 2: " ++ show (map head finalCratesPt2) ++ "\n"
    where finalCratesPt1 = foldr doMovePt1 crates moves
          finalCratesPt2 = foldr doMovePt2 crates moves
          crates = filter (not.null) (map (filter isUpper) (transpose cratesInput))
          moves = reverse (map readMove (filter (not.null) movesInput))
          (cratesInput, movesInput) = break null (lines input)

-- Some types for readability.
data Move = Move {num::Int, from::Int, to::Int} deriving (Show)
type Crates = [String]
type Stack = String

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- Parse the strings from the problem input.
readMove :: String -> Move
readMove s = Move a b c
    where a = read (takeWhile isDigit (dropWhile (not.isDigit) s))
          b = read (takeWhile isDigit (dropWhile (not.isDigit) (dropWhile isDigit (dropWhile (not.isDigit) s))))
          c = read (takeWhile isDigit (dropWhile (not.isDigit) (dropWhile isDigit (dropWhile (not.isDigit) (dropWhile isDigit (dropWhile (not.isDigit) s))))))

-- Remove a number of crates from a specific target stack.
popStack :: Move -> Crates -> (Crates, Stack)
popStack move crates = (before ++ [remaining] ++ after, removed)
    where (before, target:after) = splitAt ((from move)-1) crates
          (removed, remaining) = splitAt (num move) target

-- Place a number of crates on a specific target stack.
pushStack :: Move -> Stack -> Crates -> Crates
pushStack move moved crates = before ++ [moved ++ target] ++ after
    where (before, target:after) = splitAt ((to move)-1) crates

-- Perform a move while reversing the crates, for part 1.
doMovePt1 :: Move -> Crates -> Crates
doMovePt1 move crates = pushStack move (reverse moved) crates_
    where (crates_, moved) = popStack move crates

-- Perform a move, without reversing the crates, for part 2.
doMovePt2 :: Move -> Crates -> Crates
doMovePt2 move crates = pushStack move moved crates_
    where (crates_, moved) = popStack move crates
