-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.List (nub)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show p1 ++ "\n"
           ++ "Problem 2: " ++ show p2 ++ "\n"
    where p1 = length.nub $ (map (\(a,b,c) -> c) allPositions)
          allPositions = foldr move startPosition moves
          startPosition = [(' ', (0,0), (0,0))]
          moves = reverse (concat (map readMove (lines input)))
          p2 = 2

readMove :: String -> String
readMove s = case words s of
    [(dir:[]), dist] -> replicate (read dist) dir
    otherwise -> ""

move :: Char -> [(Char, (Int, Int), (Int, Int))] -> [(Char, (Int, Int), (Int, Int))]
move 'U' positions = ('U', (hx, hy+1), (tx+tx', ty+ty')):positions
    where (_, (hx, hy), (tx, ty)) = head positions
          tx' = case hy>ty of
                True -> hx-tx
                False -> 0
          ty' = case hy>ty of
                True -> 1
                False -> 0
move 'D' positions = ('D', (hx, hy-1), (tx+tx', ty+ty')):positions
    where (_, (hx, hy), (tx, ty)) = head positions
          tx' = case hy<ty of
                True -> hx-tx
                False -> 0
          ty' = case hy<ty of
                True -> -1
                False -> 0
move 'R' positions = ('R', (hx+1, hy), (tx+tx', ty+ty')):positions
    where (_, (hx, hy), (tx, ty)) = head positions
          tx' = case hx>tx of
                True -> 1
                False -> 0
          ty' = case hx>tx of
                True -> hy-ty
                False -> 0
move 'L' positions = ('L', (hx-1, hy), (tx+tx', ty+ty')):positions
    where (_, (hx, hy), (tx, ty)) = head positions
          tx' = case hx<tx of
                True -> -1
                False -> 0
          ty' = case hx<tx of
                True -> hy-ty
                False -> 0
move _ positions = positions

