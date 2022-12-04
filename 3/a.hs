module Aoc where

import Data.Char

prio :: Char -> Int
prio c
  | c >= 'a' && c <= 'z' = (ord c) - (ord 'a') + 1
  | c >= 'A' && c <= 'Z' = (ord c) - (ord 'A') + 27

common :: (String, String) -> Char
common (comp1,comp2) = head [ c1 | c1 <- comp1, c2 <- comp2, c1 == c2 ]

halve :: [a] -> ([a], [a])
halve xs = go xs xs
  where
    go xs []  = ([],xs)
    go (x:xs) [_] = ([x],xs)
    go (x:xs) (_:_:ys) = let (first,last) = go xs ys in (x:first, last)

aoc :: [String] -> String
aoc f = show (sum (map prio (map common (map halve f))))
