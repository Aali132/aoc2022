module Aoc where

import Data.Char

prio :: Char -> Int
prio c
  | c >= 'a' && c <= 'z' = (ord c) - (ord 'a') + 1
  | c >= 'A' && c <= 'Z' = (ord c) - (ord 'A') + 27

common :: (String, String, String) -> Char
common (elf1,elf2,elf3) = head [ c1 | c1 <- elf1, c2 <- elf2, c3 <- elf3, c1 == c2, c2 == c3 ]

group :: [String] -> [(String, String, String)]
group (x:y:z:[]) = [(x,y,z)]
group (x:y:z:xs) = [(x,y,z)] ++ group xs

aoc :: [String] -> String
aoc f = show (sum (map prio (map common (group f))))
