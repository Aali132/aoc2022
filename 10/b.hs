module Aoc where

import Data.List.Split

type Instr = Int

parseInstr :: String -> [Instr]
parseInstr "noop" = [0]
parseInstr s = 0:(read (last (words s))):[]

parse :: [String] -> [Instr]
parse s = concat (map parseInstr s)

valueAt :: [Instr] -> Int -> Int
valueAt program cycle = 1 + sum (take (cycle-1) program)

drawSprite :: (Int, Int) -> Char
drawSprite (x, crt)
  | crt >= x && crt < x + 3 = '#'
  | otherwise = ' '

aoc :: [String] -> String
aoc f = concat ([ '\n':line | line <- chunksOf 40 [ drawSprite ((valueAt program cycle), (mod cycle 40)) | cycle <- [1..240]]])
  where program = parse f
