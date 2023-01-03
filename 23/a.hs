module Aoc where

import Data.List.Index
import Data.Foldable
import Data.Maybe
import Data.List.Unique
import Data.Bits

type Map = [[Char]]
type Pos = (Int,Int)
type Dir = Int
type Elf = Pos

type State = ([Elf],Dir)

dirList = concat (repeat [(0,-1),(0,1),(-1,0),(1,0)])

width :: Map -> Int
width m = length (m !! 0)

height :: Map -> Int
height m = length m

getMap :: Map -> Pos -> Char
getMap m (x,y) = (m !! y) !! x

findElves :: Map -> [Elf]
findElves m = [ (x,y) | y <- [0..(height m)-1], x <- [0..(width m)-1], (getMap m (x,y)) == '#' ]

maskList = [(1,1),(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1),(0,1)]
maskVal = [1,2,4,8,16,32,64,128]

elfMask :: [Elf] -> Elf -> Int
elfMask e (x,y) = sum [ val | (val, (dx,dy)) <- zip maskVal maskList, elem (x+dx,y+dy) e ]

check :: [Elf] -> Elf -> Pos -> Int -> Maybe Pos
check e _ _ 0 = Nothing
check e (x,y) (0,-1) mask = if (mask .&. 28) == 0 then Just (x,y-1) else Nothing
check e (x,y) (0,1) mask = if (mask .&. 193) == 0 then Just (x,y+1) else Nothing
check e (x,y) (-1,0) mask = if (mask .&. 112) == 0 then Just (x-1,y) else Nothing
check e (x,y) (1,0) mask = if (mask .&. 7) == 0 then Just (x+1,y) else Nothing

propose :: State -> Elf -> Maybe Pos
propose (e,d) p = asum [ check e p o (elfMask e p) | o <- (take 4 (drop d dirList)) ]

nextRound :: State -> State
nextRound (e,d) = (next,d+1)
  where ps = [ propose (e,d) elf | elf <- e ]
        moves = unique ps
        next = [ elf | (elf,move) <- zip e ps, not (elem move moves) ] ++ (catMaybes moves)

bbox :: [Elf] -> (Int,Int,Int,Int)
bbox e = (minX,maxX,minY,maxY)
  where minX = minimum [ x | (x,y) <- e ]
        maxX = maximum [ x | (x,y) <- e ]
        minY = minimum [ y | (x,y) <- e ]
        maxY = maximum [ y | (x,y) <- e ]

display :: [Elf] -> String
display e = [ if x >= x0 then (if (elem (x,y) e) then '#' else '.') else '\n' | y <- [y0..y1], x <- [x0-1..x1] ]
  where (x0,x1,y0,y1) = bbox e

aoc :: [String] -> String
aoc f = show (w * h - (length e))
  where e = findElves f
        iter = iterate nextRound (e,0)
        (end,_) = iter !! 10
        (x0,x1,y0,y1) = bbox end
        w = (x1-x0)+1
        h = (y1-y0)+1
