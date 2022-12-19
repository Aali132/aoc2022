module Aoc where

import Data.List

type InputNode = (String,Int,[String])
type InputGraph = [InputNode]

type Arc = (String,Int)
type Node = (String,Int,[Arc])
type Graph = [Node]

iarcs :: InputGraph -> String -> [String]
iarcs ig n = arcs
  where (Just (_,_,arcs)) = find (\(name,_,_) -> name == n) ig

shortest :: InputGraph -> String -> String -> [String] -> Int
shortest ig from to v
  | from == to = 0
  | otherwise = 1 + minimum (9999:[ shortest ig next to (next:v) | next <- iarcs ig from, not (elem next v) ])
  
reduce :: InputGraph -> Graph
reduce ig = [ (name,flow,[ (to,(shortest ig name to [name])+1) | (to,_,_) <- nodes, to /= name ]) | (name,flow,_) <- nodes ]
  where nodes = filter (\(n,flow,_) -> flow /= 0 || n == "AA" ) ig

acost :: Graph -> String -> String -> Int
acost g from to = head [ cost | (node,cost) <- arcs, node == to ]
  where (Just (_,_,arcs)) = find (\(name,_,_) -> name == from) g

nflow :: Graph -> String -> Int
nflow g node = flow
  where (Just (_,flow,_)) = find (\(name,_,_) -> name == node) g

hflow :: Graph -> String -> String -> Int -> Int
hflow g from to time = (nflow g to) * (time - (acost g from to))

parseValve :: String -> InputNode
parseValve s = (name,flow,tunnels)
  where w = words s
        name = w !! 1
        flow = read (drop 5 (init (w !! 4)))
        tunnels = (map init (init (drop 9 w))) ++ [last w]

maxFlow :: Graph -> String -> Int -> Int
maxFlow [] _ _ = 0
maxFlow g node time
  | time > 0 = maximum (0:[ (hflow g node next time) + (maxFlow (filter (\(n,_,_) -> n /= node) g) next (time - (acost g node next))) | (next,_,_) <- g, next /= node ])
  | otherwise = 0

aoc :: [String] -> String
aoc f = show (maxFlow g "AA" 30)
  where g = reduce (map parseValve f)
