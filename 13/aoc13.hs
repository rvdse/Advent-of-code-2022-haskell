-- The program expects the input on stdin, ie
-- $ ./solve < input
import Data.Char (isDigit)

main = interact solve

solve :: String -> String
solve input = "Problem 1:\n" ++ p1 ++ "\n"
           ++ "Problem 2: " ++ show p2 ++ "\n"
    where p1 = concat $ map (\t -> show t ++ "\n") tokens
          p2 = 2
          tokens = map (parse.tokenize) (lines input)

data Token = ListStart | ListEnd | ListSeparator | Number Int deriving Show
data List = Leaf Int | List [List] deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('[':cs) = ListStart : tokenize cs
tokenize (',':cs) = ListSeparator : tokenize cs
tokenize (']':cs) = ListEnd : tokenize cs
tokenize (' ':cs) = tokenize cs
tokenize (c:cs)
    | isDigit c = Number (read (takeWhile isDigit (c:cs))) : tokenize (dropWhile isDigit (c:cs))
    | otherwise = []

parse :: [Token] -> ([Token], List)
parse ListStart:ts = l:parse rest
    where (l, rest) = parseList ts
parse [] = ([], List [])

parseList :: [Token] -> ([Token], List)
