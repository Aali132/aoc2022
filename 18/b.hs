module Aoc where

import Data.List.Split
import Data.List.Index

import qualified Data.Set as Set

type Point = (Int,Int,Int)
type Map = [[[Int]]]

getMap :: Map -> Point -> Int
getMap m (x,y,z) = ((m !! z) !! y) !! x

setMap :: Int -> Point -> Map -> Map
setMap v (x,y,z) m = setAt z (setAt y (setAt x v ((m !! z) !! y)) (m !! z)) m

incMap :: Point -> Map -> Map
incMap p m = setMap ((getMap m p)+1) p m

sides :: Point -> [Point]
sides (x,y,z) = [ (x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1) ]

openPoints :: Point -> Map -> Map
openPoints p m = foldr incMap m (sides p)

closePoints :: Map -> [Point] -> Map
closePoints m ps = foldr (setMap 0) m ps

clip :: [Point] -> [Point]
clip ps = [ (x,y,z) | (x,y,z) <- ps, x >= 0, x < 22, y >= 0, y < 22, z >= 0, z < 22 ]

type State = (Set.Set Point,Set.Set Point)

reachable :: [Point] -> State -> State
reachable cp (op0,r)
  | elem p cp = (op,r)
  | otherwise = (Set.difference (Set.union op (Set.fromList (clip (sides p)))) r,Set.insert p r)
  where (p,op) = Set.deleteFindMin op0

parse :: String -> Point
parse s = (x+1,y+1,z+1)
  where [x,y,z] = Prelude.map read (splitOn "," s)

aoc :: [String] -> String
aoc f = show (sum ((concat.concat) mr))
  where points = map parse f
        m0 = take 22 (repeat (take 22 (repeat (take 22 (repeat (0))))))
        mo = foldr openPoints m0 points
        mc = closePoints mo points
        riter = iterate (reachable points) (Set.singleton (0,0,0),Set.empty)
        ap = Set.fromList [(x,y,z) | x <- [0..21],y <- [0..21],z <- [0..21]]
        nrp = Set.toList (Set.difference ap (snd (last (takeWhile (\(op,r) -> not (null op)) riter))))
        mr = closePoints mc nrp
