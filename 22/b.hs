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

quad :: Pos -> Pos -> Pos
quad (qw,qh) (x,y) = (div x qw, div y qh)

wrapEx :: Pos -> Pos -> Pos -> Pos -> State
wrapEx (qw,qh) (2,0) (1,0) (x,y) = ((qw+y,qh),1) -- left from side 1 -> down on side 3
wrapEx (qw,qh) (2,0) (3,0) (x,y) = ((qw*4-1,(qh*3-1)-y),2) -- right from side 1 -> left on side 6

wrapEx (qw,qh) (0,1) (0,0) (x,y) = (((qw*3-1)-x,0),1) -- up from side 2 -> down on side 1
wrapEx (qw,qh) (0,1) (3,1) (x,y) = (((qw*4-1)-(y-qh),qh*3-1),3) -- left from side 2 -> up on side 6
wrapEx (qw,qh) (0,1) (0,2) (x,y) = (((qw*3-1)-x,qh*3-1),3) -- down from side 2 -> up on side 5

wrapEx (qw,qh) (1,1) (1,0) (x,y) = ((qw*2,x-qw),0) -- up from side 3 -> right on side 1
wrapEx (qw,qh) (1,1) (1,2) (x,y) = ((qw*3-1,(qh*3-1)-(x-qw)),0) -- down from side 3 -> right on side 5

wrapEx (qw,qh) (2,1) (3,1) (x,y) = (((qw*4-1)-(y-qh),qh*2),1) -- right from side 4 -> down on side 6

wrapEx (qw,qh) (2,2) (1,2) (x,y) = (((qw*2-1)-(y-qh*2),qh*2-1),3) -- left from side 5 -> up on side 3

wrapEx (qw,qh) (3,2) (3,0) (x,y) = ((0,(qh*2-1)-(x-qw*3)),0) -- down from side 6 -> right on side 2
wrapEx (qw,qh) (3,2) (3,1) (x,y) = ((qw*3-1,(qh*2-1)-(x-qw*3)),2) -- up from side 6 -> left on side 4
wrapEx (qw,qh) (3,2) (0,2) (x,y) = ((qw*3-1,(qh-1)-(y-qh*2)),2) -- right from side 6 -> left on side 1

wrapEx (qw,qh) (2,0) (2,2) (x,y) = (((qw*3-1)-x,qh),1) -- up from side 1 -> down on side 2
wrapEx (qw,qh) (2,2) (2,0) (x,y) = (((qw*3-1)-x,qh*2-1),3) -- down from side 5 -> up on side 2

wrapIn :: Pos -> Pos -> Pos -> Pos -> State
wrapIn (qw,qh) (1,0) (0,0) (x,y) = ((0,(qh*3-1)-y),0) -- left from side 1 -> right on side 3
wrapIn (qw,qh) (1,0) (1,3) (x,y) = ((0,qh*3+(x-qw*1)),0) -- up from side 1 -> right on side 2

wrapIn (qw,qh) (2,0) (0,0) (x,y) = ((qw*2-1,(qh*3-1)-y),2) -- right from side 6 -> left on side 5
wrapIn (qw,qh) (2,0) (2,1) (x,y) = ((qw*2-1,(qh-1)+(x-qw*2)),2) -- down from side 6 -> left on side 4
wrapIn (qw,qh) (2,0) (2,3) (x,y) = ((x-qw*2,qh*4-1),3) -- up from side 6 -> up on side 2

wrapIn (qw,qh) (1,1) (0,1) (x,y) = ((y-qh,qh*2),1) -- left from side 4 -> down on side 3
wrapIn (qw,qh) (1,1) (2,1) (x,y) = ((qw*2+(y-qh),qh-1),3) -- right from side 4 -> up on side 6

wrapIn (qw,qh) (0,2) (0,1) (x,y) = ((qw,qh+x),0) -- up from side 3 -> right on side 4
wrapIn (qw,qh) (0,2) (2,2) (x,y) = ((qw,(qh-1)-(y-qh*2)),0) -- left from side 3 -> right on side 1

wrapIn (qw,qh) (1,2) (2,2) (x,y) = ((qw*3-1,(qh-1)-(y-qh*2)),2) -- right from side 5 -> left on side 6
wrapIn (qw,qh) (1,2) (1,3) (x,y) = ((qw-1,qh*3+(x-qw)),2) -- down from side 5 -> left on side 2

wrapIn (qw,qh) (0,3) (0,0) (x,y) = ((x+qw*2,0),1) -- down from side 2 -> down on side 6
wrapIn (qw,qh) (0,3) (1,3) (x,y) = ((qw+(y-qh*3),qh*3-1),3) -- right from side 2 -> up on side 5
wrapIn (qw,qh) (0,3) (2,3) (x,y) = ((qw+(y-qh*3),0),1) -- left from side 2 -> down on side 1

seek :: Map -> State -> Pos -> State
seek m ((x,y),d) (a,b)
  | getMap m (nx,ny) == ' ' || mx /= nx || my /= ny = wrap (qw,qh) (quad (qw,qh) (x,y)) (quad (qw,qh) (nx,ny)) (x,y)
  | otherwise = ((nx,ny),d)
  where nx = cmod mx (width m)
        mx = x+a
        ny = cmod my (height m)
        my = y+b
        dx = if (width m) == 16 then 4 else 3
        dy = if (width m) == 16 then 3 else 4
        qw = div (width m) dx
        qh = div (height m) dy
        wrap = if (width m) == 16 then wrapEx else wrapIn

nextPos :: Map -> State -> State
nextPos m (p,0) = seek m (p,0) (1,0)
nextPos m (p,1) = seek m (p,1) (0,1)
nextPos m (p,2) = seek m (p,2) (-1,0)
nextPos m (p,3) = seek m (p,3) (0,-1)

followInstr :: Map -> State -> Instr -> State
followInstr m (p,d) L = (p,cmod (d-1) 4)
followInstr m (p,d) R = (p,cmod (d+1) 4)
followInstr m (p,d) (M 0) = (p,d)
followInstr m (p,d) (M n)
  | nt == '#' = (p,d)
  | nt == '.' = followInstr m (np,nd) (M (n-1))
  where (np,nd) = nextPos m (p,d)
        nt = getMap m np

parseInstr :: String -> [Instr]
parseInstr [] = []
parseInstr ('R':s) = (R:(parseInstr s))
parseInstr ('L':s) = (L:(parseInstr s))
parseInstr s = ((M (fst (head rs))):(parseInstr (snd (head rs))))
  where rs = reads s

dir :: Dir -> Char
dir 0 = '>'
dir 1 = 'v'
dir 2 = '<'
dir 3 = '^'

renderPath :: Map -> [State] -> Map
renderPath m0 s = foldl (\m (p,d) -> setMap (dir d) p m) m0 s

display :: Map -> String
display m = concat (map (\x -> ('\n':x)) m)

aoc :: [String] -> String
aoc f = show ((ey+1)*1000+(ex+1)*4+ed)
--aoc f = display render
  where rm = init (init f)
        m = [ take (maximum (map length rm)) (line ++ (repeat ' ')) | line <- rm ]
        inst = parseInstr (last f)
        start = if (width m) == 16 then ((div (width m) 2,0),0) else ((div (width m) 3,0),0)
        path = scanl (followInstr m) start inst
        render = renderPath m path
        ((ex,ey),ed) = last path
