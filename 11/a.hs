module Aoc where

import Data.List.Index
import Data.List.Split
import Data.List.Utils
import Data.List

type Items = [Int]
type Inspect = Int -> (Int,Int)
type Monkey = (Items,Inspect,Int)

instance Show (Inspect) where
  show i = "<inspect>"

parseExpr :: (String, String) -> (Int -> Int)
parseExpr ("+","old") = \x -> div (x + x) 3
parseExpr ("*","old") = \x -> div (x * x) 3
parseExpr ("+",y) = \x -> div (x + (read y)) 3
parseExpr ("*",y) = \x -> div (x * (read y)) 3

parseTest :: (String, String, String) -> (Int -> Int)
parseTest (m,t,f) = \x -> if (mod x (read m) == 0) then (read t) else (read f)

parseMonkey :: [String] -> Monkey
parseMonkey (_:items:oper:test:monkt:monkf:_) = (map read (map (replace "," "") (drop 2 (words items))),\x -> (opExpr x, testExpr (opExpr x)),0)
  where opExpr = parseExpr (last (init (words oper)), last (words oper))
        testExpr = parseTest (last (words test), last (words monkt), last (words monkf))

addItem :: Int -> Monkey -> Monkey
addItem i (items,inspect,count) = (items ++ [i],inspect,count)

giveItem :: [Monkey] -> (Int,Int) -> [Monkey]
giveItem m (value,index) = modifyAt index (addItem value) m

throwAll :: Monkey -> ([(Int,Int)], Monkey)
throwAll (items,inspect,count) = ((map inspect items), ([],inspect,count + (length items)))

nextMonkey :: [Monkey] -> Int -> [Monkey]
nextMonkey m index = foldl giveItem (setAt index monkey m) items
  where (items,monkey) = throwAll (m !! index)

nextRound :: [Monkey] -> [Monkey]
nextRound m = foldl nextMonkey m [0..(length m)-1]

aoc :: [String] -> String
aoc f = show (product (take 2 (reverse (sort [ count | (items,inspect,count) <- ((iterate nextRound (map parseMonkey (chunksOf 7 f))) !! 20) ]))))
