-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.Char (isDigit)
import Data.List (nub)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show p1 ++ "\n"
           ++ "Problem 2: " ++ show p2 ++ "\n"
           ++ drawnMap ++ "\n"
    where p1 = length (filter (\c -> elem c "#S") (rowToStr sensors beacons ranges targetRow))
          p2 = tuningFreq (fst (head distressBeacon))
          distressMap = map readInput (lines input)
          sensors = map (\(x, y, _, _) -> (x, y)) distressMap
          beacons = map (\(_, _, x, y) -> (x, y)) distressMap
          ranges = map toRange distressMap
          isExample = length input == 738
          drawnMap = if isExample then toStr sensors beacons ranges else ""
          targetRow = if isExample then 10 else 2000000
          lim = if isExample then 20 else 4000000
          distressCandidates = [(x,y) | x<-[0..lim], y<-[0..lim]]
          unscanned = filter (\(x,y)->x>=0 && x<=lim && y>=0 && y<=lim) (concat (map around ranges))
          distressBeacon = filter (not.snd) (zip unscanned (map (inRange ranges) unscanned))

-- Return all points on a circle (or diamond) outside the range of a sensor.
around :: (Int, Int, Int) -> [(Int, Int)]
around (sx, sy, r) = [(x,y) | x <- [sx-r-1 .. sx+r+1],
                              y <- [sy-r-1 .. sy+r+1],
                              abs(x-sx+y-sy) == r+1]

toRange :: (Int, Int, Int, Int) -> (Int, Int, Int)
toRange (sx, sy, bx, by) = (sx, sy, manhattan (sx, sy) (bx, by))

readInput :: String -> (Int, Int, Int, Int)
readInput s = (sxi, syi, bxi, byi)
    where (sensor, beacon) = break (==':') s
          (sx, sy) = break (==',') sensor
          (bx, by) = break (==',') beacon
          [sxi, syi, bxi, byi] = map (read.(filter (\c -> isDigit c || c == '-'))) [sx, sy, bx, by]

toStr :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int)] -> String
toStr sensors beacons ranges = concat (map (rowToStr sensors beacons ranges) [(-2 + minimum all_y) .. (2 + maximum all_y)])
    where all_y = map snd (sensors ++ beacons)
    -- where all_y = foldr (\(_, y1, _, y2) acc -> y1:y2:acc) [] dat

rowToStr :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int)] -> Int -> String
rowToStr sensors beacons ranges n = lineNumber ++ map (toChar sensors beacons ranges n) [(-2 + minimum all_x) .. (2 + maximum all_x)] ++ "\n"
    where all_x = map fst (sensors ++ beacons)
          lineNumber = take (2 - length (show n)) (cycle " ") ++ show n ++ " "
    -- where all_x = foldr (\(x1, _, x2, _) acc -> x1:x2:acc) [] dat

toChar :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int)] -> Int -> Int -> Char
toChar sensors beacons ranges y x
    | elem (x, y) sensors = 'S'
    | elem (x, y) beacons = 'B'
    | inRange ranges (x, y) = '#'
    | otherwise = '.'

inRange :: [(Int, Int, Int)] -> (Int, Int) -> Bool
inRange ranges pos = any testRange ranges
    where testRange (sx,sy,r) = manhattan pos (sx, sy) <= r

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

tuningFreq :: (Int, Int) -> Int
tuningFreq (x,y) = x*4000000+y

