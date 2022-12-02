module Aoc where

data Move = Rock | Paper | Scissors deriving Show

theirMove :: Char -> Move
theirMove 'A' = Rock
theirMove 'B' = Paper
theirMove 'C' = Scissors

myMove :: Char -> Move
myMove 'X' = Rock
myMove 'Y' = Paper
myMove 'Z' = Scissors

moves :: String -> (Move, Move)
moves (t:_:m:[]) = (theirMove t, myMove m)

parse :: [String] -> [(Move, Move)]
parse s = map moves s

score :: (Move, Move) -> Integer
score (Rock, Rock) = 1+3
score (Paper, Rock) = 1+0
score (Scissors, Rock) = 1+6
score (Rock, Paper) = 2+6
score (Paper, Paper) = 2+3
score (Scissors, Paper) = 2+0
score (Rock, Scissors) = 3+0
score (Paper, Scissors) = 3+6
score (Scissors, Scissors) = 3+3

aoc :: [String] -> String
aoc f = show (sum (map score (parse f)))
