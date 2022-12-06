-- The program expects the input on stdin, ie
-- $ ./solve < input

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show p1 ++ "\n"
           ++ "Problem 2: " ++ show p2 ++ "\n"
    where p1 = length (filter fullOverlap intervals)
          p2 = length (filter partialOverlap intervals)
          intervals = map getIntervals (lines input)

fullOverlap :: (Int, Int, Int, Int) -> Bool
fullOverlap (a, b, c, d)
    | (a <= c && b >= d) || (a >= c && b <= d) = True
    | otherwise = False

partialOverlap :: (Int, Int, Int, Int) -> Bool
partialOverlap (a, b, c, d)
    | (b < c) || (a > d) = False
    | otherwise = True

getIntervals :: String -> (Int, Int, Int, Int)
getIntervals s = (a, b, c, d)
    where (a, b) = parseInterval (takeWhile (/=',') s)
          (c, d) = parseInterval (tail (dropWhile (/=',') s))
          parseInterval = (\s -> (
              read (takeWhile (/='-') s),
              read (tail (dropWhile (/='-') s))))

