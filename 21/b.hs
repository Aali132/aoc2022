module Aoc where

import Data.List

data Expr = Num String Int | Add String String String | Sub String String String | Mul String String String | Div String String String deriving (Eq, Show)

findDef :: String -> Expr -> Bool
findDef d (Num x _) = x == d
findDef d (Add x _ _) = x == d
findDef d (Sub x _ _) = x == d
findDef d (Mul x _ _) = x == d
findDef d (Div x _ _) = x == d

findExpr :: [Expr] -> String -> Expr
findExpr e x = expr
  where Just expr = find (findDef x) e

subExpr :: Expr -> (String,String)
subExpr (Add _ a b) = (a,b)
subExpr (Sub _ a b) = (a,b)
subExpr (Mul _ a b) = (a,b)
subExpr (Div _ a b) = (a,b)

eval :: [Expr] -> String -> Int
eval e x = case (findExpr e x) of
             Num _ n -> n
             Add _ a b -> (eval e a) + (eval e b)
             Sub _ a b -> (eval e a) - (eval e b)
             Mul _ a b -> (eval e a) * (eval e b)
             Div _ a b -> (eval e a) `div` (eval e b)

parse :: String -> Expr
parse s
  | (length w) == 2 = Num x (read (w !! 1))
  | (w !! 2) == "+" = Add x (w !! 1) (w !! 3)
  | (w !! 2) == "-" = Sub x (w !! 1) (w !! 3)
  | (w !! 2) == "*" = Mul x (w !! 1) (w !! 3)
  | (w !! 2) == "/" = Div x (w !! 1) (w !! 3)
  where w = words s
        x = init (w !! 0)

invertA :: Expr -> Expr
invertA (Num x n) = Num x n
invertA (Add x a b) = Sub a x b
invertA (Sub x a b) = Add a x b
invertA (Mul x a b) = Div a x b
invertA (Div x a b) = Mul a x b

invertB :: Expr -> Expr
invertB (Num x n) = Num x n
invertB (Add x a b) = Sub b x a
invertB (Sub x a b) = Sub b a x
invertB (Mul x a b) = Div b x a
invertB (Div x a b) = Div b a x

invert :: [Expr] -> [Expr]
invert e = [ n | n@(Num _ _) <- elist ] ++ elist
  where elist = concat [ [invertA expr] ++ [invertB expr] | expr <- e ]

isNum :: [Expr] -> String -> Bool
isNum e "humn" = False
isNum e x = case (findExpr e x) of
             Num _ _ -> True
             _ -> False

reduceExpr :: [Expr] -> Expr -> Expr
reduceExpr e expr@(Num _ _) = expr
reduceExpr e expr@(Add x a b) = if (isNum e a) && (isNum e b) then Num x (eval e x) else expr
reduceExpr e expr@(Sub x a b) = if (isNum e a) && (isNum e b) then Num x (eval e x) else expr
reduceExpr e expr@(Mul x a b) = if (isNum e a) && (isNum e b) then Num x (eval e x) else expr
reduceExpr e expr@(Div x a b) = if (isNum e a) && (isNum e b) then Num x (eval e x) else expr

reduce :: [Expr] -> [Expr]
reduce e = map (reduceExpr e) e

aoc :: [String] -> String
aoc f = (show (eval ev "humn"))
  where er = map parse f
        (a,b) = subExpr (findExpr e "root")
        einv = invert (delete (findExpr e "humn") e)
        ecln = delete (findExpr einv a) einv
        ev = ((Num a (eval e b)):ecln)
        riter = iterate reduce er
        e = riter !! 50
