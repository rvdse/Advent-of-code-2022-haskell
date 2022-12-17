-- The program expects the input on stdin, ie
-- $ ./solve < input
import Control.Arrow ((&&&))
import Data.Char (isDigit, isUpper)
import Data.List (sort)
import Debug.Trace (trace)

main = interact solve

solve :: String -> String
solve input = "Problem 1: " ++ show p1 ++ "\n"
           ++ "Problem 2: " ++ show p2 ++ "\n"
    where p1 = doSomething valves startState
          p2 = score valves p1
          -- p2 = score valves ("CC", 30, [("DD", 2), ("BB", 5), ("JJ", 9), ("HH", 17), ("EE", 21), ("CC", 24)])
          valves = readInput (lines input)
          startState = ("AA", 1, []) :: State

type Valve = (
    String, -- label
    Int, -- flow rate
    [String]) -- list of connections
type State = (
    String, -- current position
    Int, -- current time
    [(String, Int)]) -- state of each valve: (label, time_of_open)

-- Read input string and format as list of Valve.
readInput :: [String] -> [Valve]
readInput [] = []
readInput (s:ss) = case words s of
    _:label:_:_:rawRate:_:_:_:_:rawConns ->
            (label, rate, conns):readInput ss
        where conns = map (filter isUpper) rawConns
              rate = read (filter isDigit rawRate)
    otherwise -> []

doSomething :: [Valve] -> State -> State
doSomething _ s@(_,30,_) = s -- Time's up!
-- doSomething vs s = snd.head.sort.map (score &&& id) $ alts
doSomething vs s = trace (show bestAlt) (doSomething vs bestAlt)
    where bestAlt = snd (last (sort (zip (map (score vs) alts) alts)))
          altsScored = sort (zip (map (score vs) alts) alts)
          alts = moves ++ [open s]
          moves = map (move s) (findPossibleMoves vs s)

findPossibleMoves :: [Valve] -> State -> [String]
findPossibleMoves [] (_,_,_) = []
findPossibleMoves ((lbl,_,cs):vs) s@(pos,_,_)
    | lbl==pos = cs
    | otherwise = findPossibleMoves vs s

-- Open valve at current position.
open :: State -> State
open (pos, t, os)
    | elem pos (map fst os) = (pos, t+1, os) -- already open
    | otherwise = (pos, t+1, (pos, t):os)

-- Move to a new position. Does not check if valid.
move :: State -> String -> State
move (pos, t, os) dest = (dest, t+1, os)

-- Calculate pressure score at minute 30.
score :: [Valve] -> State -> Int
score [] _ = 0
score ((lbl,rate,_):vs) s = rate*time + score vs s
    where time = 30 - (opened s lbl)

-- Given state, when was a valve opened?
opened :: State -> String -> Int
opened (_,_,o) l = openHelper o l
    where openHelper [] _ = 30
          openHelper ((ol,ot):os) l
            | ol==l = ot
            | otherwise = openHelper os l

