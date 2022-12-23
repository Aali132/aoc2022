module Aoc where

import Data.List.Index

type Map = [[Char]]
type Pos = (Int,Int)
type Dir = Int

type State = (Pos,Dir)

data Instr = M Int | L | R deriving Show

cmod :: Int -> Int -> Int
cmod a b
  | m < 0 = m + b
  | otherwise = m
  where m = mod a b

width :: Map -> Int
width m = length (m !! 0)

height :: Map -> Int
height m = length m

getMap :: Map -> Pos -> Char
getMap m (x,y) = (m !! y) !! x

setMap :: Char -> Pos -> Map -> Map
setMap v (x,y) m = setAt y (setAt x v (m !! y)) m

seek :: Map -> Pos -> Pos -> Pos
seek m (x,y) (a,b)
  | getMap m (nx,ny) == ' ' = seek m (nx,ny) (a,b)
  | otherwise = (nx,ny)
  where nx = cmod (x+a) (width m)
        ny = cmod (y+b) (height m)

nextPos :: Map -> Pos -> Dir -> Pos
nextPos m p 0 = seek m p (1,0)
nextPos m p 1 = seek m p (0,1)
nextPos m p 2 = seek m p (-1,0)
nextPos m p 3 = seek m p (0,-1)

followInstr :: Map -> State -> Instr -> State
followInstr m (p,d) L = (p,cmod (d-1) 4)
followInstr m (p,d) R = (p,cmod (d+1) 4)
followInstr m (p,d) (M 0) = (p,d)
followInstr m (p,d) (M n)
  | nt == '#' = (p,d)
  | nt == '.' = followInstr m (np,d) (M (n-1))
  where np = nextPos m p d
        nt = getMap m np

parseInstr :: String -> [Instr]
parseInstr [] = []
parseInstr ('R':s) = (R:(parseInstr s))
parseInstr ('L':s) = (L:(parseInstr s))
parseInstr s = ((M (fst (head rs))):(parseInstr (snd (head rs))))
  where rs = reads s

aoc :: [String] -> String
aoc f = show((ey+1)*1000+(ex+1)*4+ed)
  where rm = init (init f)
        m = [ take (maximum (map length rm)) (line ++ (repeat ' ')) | line <- rm ]
        inst = parseInstr (last f)
        start = (nextPos m (0,0) 0, 0)
        ((ex,ey),ed) = foldl (followInstr m) start inst
