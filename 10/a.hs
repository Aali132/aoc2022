module Aoc where

type Instr = Int

parseInstr :: String -> [Instr]
parseInstr "noop" = [0]
parseInstr s = 0:(read (last (words s))):[]

parse :: [String] -> [Instr]
parse s = concat (map parseInstr s)

signalAt :: [Instr] -> Int -> Int
signalAt program cycle = (1 + sum (take (cycle-1) program)) * cycle

aoc :: [String] -> String
aoc f = show (sum (map (signalAt program) [20,60,100,140,180,220]))
  where program = parse f
