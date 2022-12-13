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
parseExpr ("+","old") = \x -> x + x
parseExpr ("*","old") = \x -> x * x
parseExpr ("+",y) = \x -> x + (read y)
parseExpr ("*",y) = \x -> x * (read y)

parseTest :: (String, String, String) -> (Int -> Int)
parseTest (m,t,f) = \x -> if (mod x (read m) == 0) then (read t) else (read f)

parseMonkey :: [String] -> (Int, Monkey)
parseMonkey (_:items:oper:test:monkt:monkf:_) = (read (last (words test)),(map read (map (replace "," "") (drop 2 (words items))),\x -> (opExpr x, testExpr (opExpr x)),0))
  where opExpr = parseExpr (last (init (words oper)), last (words oper))
        testExpr = parseTest (last (words test), last (words monkt), last (words monkf))

addItem :: Int -> Monkey -> Monkey
addItem i (items,inspect,count) = (items ++ [i],inspect,count)

giveItem :: Int -> [Monkey] -> (Int,Int) -> [Monkey]
giveItem gcd m (value,index) = modifyAt index (addItem (mod value gcd)) m

throwAll :: Monkey -> ([(Int,Int)], Monkey)
throwAll (items,inspect,count) = ((map inspect items), ([],inspect,count + (length items)))

nextMonkey :: Int -> [Monkey] -> Int -> [Monkey]
nextMonkey gcd m index = foldl (giveItem gcd) (setAt index monkey m) items
  where (items,monkey) = throwAll (m !! index)

nextRound :: Int -> [Monkey] -> [Monkey]
nextRound gcd m = foldl (nextMonkey gcd) m [0..(length m)-1]

aoc :: [String] -> String
aoc f = show (product (take 2 (reverse (sort [ count | (items,inspect,count) <- ((iterate (nextRound gcd) (map snd monkeys)) !! 10000) ]))))
  where monkeys = map parseMonkey (chunksOf 7 f)
        gcd = product (map fst monkeys)
