module Aoc where

import Data.List.Index
import Data.Foldable
import Data.Maybe
import Data.List.Unique
import Data.Bits

import qualified Data.HashSet as HashSet

type Map = [[Char]]
type Pos = (Int,Int)
type Dir = Int
type Elf = Pos

type Elves = HashSet.HashSet Elf

type State = (Elves,Dir,Bool)

dirList = concat (repeat [(0,-1),(0,1),(-1,0),(1,0)])

width :: Map -> Int
width m = length (m !! 0)

height :: Map -> Int
height m = length m

getMap :: Map -> Pos -> Char
getMap m (x,y) = (m !! y) !! x

findElves :: Map -> Elves
findElves m = HashSet.fromList [ (x,y) | y <- [0..(height m)-1], x <- [0..(width m)-1], (getMap m (x,y)) == '#' ]

maskList = [(1,1),(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1),(0,1)]
maskVal = [1,2,4,8,16,32,64,128]

elfMask :: Elves -> Elf -> Int
elfMask e (x,y) = sum [ val | (val, (dx,dy)) <- zip maskVal maskList, HashSet.member (x+dx,y+dy) e ]

check :: Elf -> Pos -> Int -> Maybe Pos
check _ _ 0 = Nothing
check (x,y) (0,-1) mask = if (mask .&. 28) == 0 then Just (x,y-1) else Nothing
check (x,y) (0,1) mask = if (mask .&. 193) == 0 then Just (x,y+1) else Nothing
check (x,y) (-1,0) mask = if (mask .&. 112) == 0 then Just (x-1,y) else Nothing
check (x,y) (1,0) mask = if (mask .&. 7) == 0 then Just (x+1,y) else Nothing

propose :: Elves -> Dir -> Elf -> Maybe Pos
propose e d p = asum [ check p o (elfMask e p) | o <- (take 4 (drop d dirList)) ]

nextRound :: State -> State
nextRound (e,d,_) = (next,d+1,done)
  where ps = [ propose e d elf | elf <- HashSet.toList e ]
        moves = unique ps
        next = HashSet.fromList ([ elf | (elf,move) <- zip (HashSet.toList e) ps, not (elem move moves) ] ++ (catMaybes moves))
        done = catMaybes moves == []

bbox :: Elves -> (Int,Int,Int,Int)
bbox e = (minX,maxX,minY,maxY)
  where minX = minimum [ x | (x,y) <- HashSet.toList e ]
        maxX = maximum [ x | (x,y) <- HashSet.toList e ]
        minY = minimum [ y | (x,y) <- HashSet.toList e ]
        maxY = maximum [ y | (x,y) <- HashSet.toList e ]

display :: Elves -> String
display e = [ if x >= x0 then (if (elem (x,y) e) then '#' else '.') else '\n' | y <- [y0..y1], x <- [x0-1..x1] ]
  where (x0,x1,y0,y1) = bbox e

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

aoc :: [String] -> String
aoc f = show (it)
  where e = findElves f
        iter = iterate nextRound (e,0,False)
        ((_,_,_),it) = last (takeWhileOneMore (\((_,_,d),_) -> not d) (zip iter [0..]))
