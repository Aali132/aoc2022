module Aoc where

data Move = Rock | Paper | Scissors deriving Show
data Result = Lose | Draw | Win deriving Show

theirMove :: Char -> Move
theirMove 'A' = Rock
theirMove 'B' = Paper
theirMove 'C' = Scissors

result :: Char -> Result
result 'X' = Lose
result 'Y' = Draw
result 'Z' = Win

moves :: (Move, Result) -> (Move, Move)
moves (x, Draw) = (x, x)
moves (Rock, Lose) = (Rock, Scissors)
moves (Rock, Win) = (Rock, Paper)
moves (Paper, Lose) = (Paper, Rock)
moves (Paper, Win) = (Paper, Scissors)
moves (Scissors, Lose) = (Scissors, Paper)
moves (Scissors, Win) = (Scissors, Rock)

parseLine :: String -> (Move, Result)
parseLine (t:_:r:[]) = (theirMove t, result r)

parse :: [String] -> [(Move, Result)]
parse s = map parseLine s

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
aoc f = show (sum (map (score.moves) (parse f)))
