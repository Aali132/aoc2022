module Aoc where

import Data.List

data Expr = Num String Int | Add String String String | Sub String String String | Mul String String String | Div String String String deriving Show

findDef :: String -> Expr -> Bool
findDef d (Num x _) = x == d
findDef d (Add x _ _) = x == d
findDef d (Sub x _ _) = x == d
findDef d (Mul x _ _) = x == d
findDef d (Div x _ _) = x == d

eval :: [Expr] -> String -> Int
eval e x = case expr of
             Num _ n -> n
             Add _ a b -> (eval e a) + (eval e b)
             Sub _ a b -> (eval e a) - (eval e b)
             Mul _ a b -> (eval e a) * (eval e b)
             Div _ a b -> (eval e a) `div` (eval e b)
  where Just expr = find (findDef x) e

parse :: String -> Expr
parse s
  | (length w) == 2 = Num x (read (w !! 1))
  | (w !! 2) == "+" = Add x (w !! 1) (w !! 3)
  | (w !! 2) == "-" = Sub x (w !! 1) (w !! 3)
  | (w !! 2) == "*" = Mul x (w !! 1) (w !! 3)
  | (w !! 2) == "/" = Div x (w !! 1) (w !! 3)
  where w = words s
        x = init (w !! 0)

aoc :: [String] -> String
aoc f = show (eval e "root")
  where e = map parse f
