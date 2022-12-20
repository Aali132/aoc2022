module Aoc where

import Data.List.Index
import Data.List

type Map = [[Int]]
type Pos = (Int,Int)
type Rock = Int

rock1 = (4, 1, 4, reverse [[1,1,1,1]])
rock2 = (3, 3, 5, reverse [[0,1,0],[1,1,1],[0,1,0]])
rock3 = (3, 3, 5, reverse [[0,0,1],[0,0,1],[1,1,1]])
rock4 = (1, 4, 4, reverse [[1],[1],[1],[1]])
rock5 = (2, 2, 4, reverse [[1,1],[1,1]])

rocks = [rock1, rock2, rock3, rock4, rock5]

width :: Map -> Int
width m = length (m !! 0)

height :: Map -> Int
height m = length m

getMap :: Map -> Pos -> Int
getMap m (x,y) = (m !! y) !! x

setMap :: Int -> Pos -> Map -> Map
setMap v (x,y) m = setAt y (setAt x v (m !! y)) m

move :: Map -> Pos -> Rock -> Bool
move m (x0,y0) r = sum [ 1 - (getMap m (x,y)) | x <- [x0..x0+w-1], y <- [y0..y0+h-1], x >= 0, x < 7, getMap tiles ((x-x0),(y-y0)) == 1 ] == mass
  where (w,h,mass,tiles) = rocks !! r

stop :: Map -> Pos -> Rock -> Bool
stop m (x0,y0) r = or [ y < 0 || getMap m (x,y) == 1 | x <- [x0..x0+w-1], y <- [y0..y0+h-1], getMap tiles ((x-x0),(y-y0)) == 1 ]
  where (w,h,_,tiles) = rocks !! r

type Jet = [Int]
type State = (Map,Jet,Rock,Int)
type RockState = (Int,Pos)

pushRock :: State -> RockState -> RockState
pushRock (m,jet,ri,_) (ji,(x,y))
  | move m target ri = (nji,target)
  | otherwise = (nji,(x,y))
  where target = (x+(jet !! ji),y)
        nji = (mod (ji+1) (length jet))

fallRock :: State -> RockState -> (Bool,RockState)
fallRock (m,_,ri,_) (ji,(x,y))
  | stop m target ri = (False,(ji,(x,y)))
  | otherwise = (True,(ji,target))
  where target = (x,y-1)

moveRock :: State -> (Bool,RockState) -> (Bool,RockState)
moveRock s (_,p) = fallRock s (pushRock s p)

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

placeRock :: Map -> Pos -> Int -> Map
placeRock m (x0,y0) ri = extendMap (newmap) ((maxHeight newmap) + 4)
  where (w,h,_,tiles) = rocks !! ri
        newmap = foldr (setMap 1) m [ (x,y) | x <- [x0..x0+w-1], y <- [y0..y0+h-1], getMap tiles ((x-x0),(y-y0)) == 1 ]

maxHeight :: Map -> Int
maxHeight m = (last (0:(findIndices (\x -> x) [ (sum level) > 0 | level <- m ]))) + 1

extendMap :: Map -> Int -> Map
extendMap m y = take y (m ++ (repeat (take 7 (repeat 0))))

nextRock :: State -> State
nextRock (m,jet,ri,ji) = (placeRock newmap rockPos ri,jet,mod (ri+1) (length rocks),jnew)
  where (jnew,rockPos) = snd (last (takeWhileOneMore fst (iterate (moveRock (newmap,jet,ri,0)) (True,(ji,(2,(height m) - 1))))))
        (_,_,_,tiles) = rocks !! ri
        newmap = extendMap m ((height m) + (height tiles))

type Cache = ([Int],Int,Int)

findCycle :: State -> [Cache] -> (Int,[Cache])
findCycle (m,jet,ri,ji) cs
  | (length cycle) > 1 = ((head cycle)+1, cs)
  | otherwise = (0, nc:cs)
  where nc = (m !! ((maxHeight m) - 1), ri, ji)
        cycle = elemIndices nc cs

parseJet '<' = -1
parseJet '>' = 1

parse :: String -> Jet
parse s = map parseJet s

aoc :: [String] -> String
aoc f = show ((cs-1) * (toInteger ch) + (toInteger (th !! ((fromInteger ((toInteger cycle)+cm))))))
  where jet = parse (head f)
        iter = iterate nextRock (extendMap [] 4, jet, 0, 0)
        cycle = last [ l | (l,_) <- (takeWhileOneMore (\(l,c) -> l == 0) (scanl (\(l,c) s -> (findCycle s c) ) (0,[]) iter)) ]
        th = [ maxHeight m | (m,jet,ri,ji) <- iter ]
        ch = (th !! (cycle * 3)) - (th !! (cycle * 2))
        (cs,cm) = divMod 1000000000000 (toInteger cycle)
