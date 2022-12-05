module Aoc where

import Data.List
import Data.List.Index
import Data.List.Split
import Data.Maybe

type Stack = [Char]

parseBox :: String -> Maybe Char
parseBox ('[':c:']':_) = Just c
parseBox _ = Nothing

parseState :: [String] -> [Stack]
parseState s = [ stack | stack <- [ catMaybes box | box <- (map.map) parseBox [ stack | stack <- transpose (map (chunksOf 4) s) ]], stack /= [] ]

type Move = (Int, Int, Int)

parseMoves :: [String] -> [Move]
parseMoves s = [ (read count, (read from) - 1, (read to) - 1) | ("move":count:"from":from:"to":to:[]) <- map words s]

parse :: [[String]] -> ([Stack],[Move])
parse (stacks:moves:[]) = (parseState stacks, parseMoves moves)

executeMove :: [Stack] -> Move -> [Stack]
executeMove stacks (count,from,to) = modifyAt from (\x -> drop count x) (modifyAt to (\x -> (reverse (take count (stacks !! from))) ++ x) stacks)

execute :: ([Stack],[Move]) -> [Stack]
execute (stacks,(move:moves)) = execute (executeMove stacks move,moves)
execute (stacks,[]) = stacks

aoc :: [String] -> String
aoc f = map head (execute (parse (splitOn [""] f)))
