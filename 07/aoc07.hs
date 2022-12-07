-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.List (isPrefixOf, nub, sortBy)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show p1 ++ "\n"
           ++ "Problem 2: " ++ show p2 ++ "\n"
    where p2 = snd (head (sortBy (\(_,a) (_,b) -> compare a b) bigEnoughToDelete))
          bigEnoughToDelete = filter (\d -> snd d>=neededSpace) allSizes
          neededSpace = 30000000 - 70000000 + snd (head (filter (\d -> fst d == "/") allSizes))
          p1 = sum (filter (<=100000) (map snd allSizes))
          allSizes = map (\d -> (d, calcSize fullPathFiles d)) allDirs
          allDirs = "/" : nub (map fst (filter (\x -> snd x==0) fullPathFiles))
          fullPathFiles = listFullPath [] (lines input)

-- Calculate total size of a directory.
calcSize :: [(String, Int)] -> String -> Int
calcSize x path = sum (map snd (filter (\s -> path `isPrefixOf` (fst s)) x))

-- Create a list of (fullpath, size).
listFullPath :: String -> [String] -> [(String, Int)]
listFullPath cwd (l:ls) = case words l of
    ("$":"cd":"/":[]) -> listFullPath "/" ls
    ("$":"cd":"..":[]) -> listFullPath (dropWhile (/='/') (tail cwd)) ls
    ("$":"cd":dir:[]) -> listFullPath ("/" ++ reverse dir ++ cwd) ls
    ("$":"ls":[]) -> listFullPath cwd ls
    ("dir":dir:[]) -> (reverse cwd ++ dir, 0):listFullPath cwd ls
    (size:file:[]) -> (reverse cwd ++ file, read size):listFullPath cwd ls
    otherwise -> listFullPath cwd ls
listFullPath _ [] = []
