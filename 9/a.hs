module Aoc where

import Data.List

type Move = (Char,Int)
type Pos = (Int,Int)

parse :: [String] -> [Move]
parse s = [ (dir,read len) | dir:_:len <- s ]

move :: (Move,Pos) -> [Pos]
move (('L',len),(x0,y0)) = [ (x,y0) | x <- [x0..x0+len]]
move (('R',len),(x0,y0)) = [ (x,y0) | x <- reverse [x0-len..x0]]
move (('U',len),(x0,y0)) = [ (x0,y) | y <- reverse [y0-len..y0]]
move (('D',len),(x0,y0)) = [ (x0,y) | y <- [y0..y0+len]]

moveHead :: (Pos,[Move]) -> [Pos]
moveHead (p,(m:ms)) = res ++ moveHead (last res, ms)
  where res = move (m,p)
moveHead (p,[]) = []

tailMove :: Pos -> Pos
tailMove (2, y) = (1,y)
tailMove (-2, y) = (-1,y)
tailMove (x, 2) = (x,1)
tailMove (x, -2) = (x,-1)
tailMove (_,_) = (0,0)

moveTail :: (Pos,[Pos]) -> [Pos]
moveTail ((tx,ty),((hx,hy):hs)) = (tx+mx,ty+my):(moveTail ((tx+mx,ty+my), hs))
  where (mx,my) = tailMove (hx-tx,hy-ty)
moveTail (p,[]) = [p]

aoc :: [String] -> String
aoc f = show (length (nub (moveTail ((0,0),(moveHead ((0,0),parse f))))))
