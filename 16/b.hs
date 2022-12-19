module Aoc where

import Data.List
import qualified Data.PQueue.Prio.Max as PQ

type InputNode = (String,Int,[String])
type InputGraph = [InputNode]

type Node = Int
type Graph = ([Int],[[Int]])

iarcs :: InputGraph -> String -> [String]
iarcs ig n = arcs
  where (Just (_,_,arcs)) = find (\(name,_,_) -> name == n) ig

shortest :: InputGraph -> String -> String -> [String] -> Int
shortest ig from to v
  | from == to = 1
  | otherwise = 1 + minimum (9999:[ shortest ig next to (next:v) | next <- iarcs ig from, not (elem next v) ])
  
reduce :: InputGraph -> (Node,Int,Graph)
reduce ig = (head (findIndices (\(n,_,_) -> n == "AA") nodes),length nodes,(map (\(_,f,_) -> f) nodes, [ [ shortest ig from to [from] | (to,_,_) <- nodes ] | (from,_,_) <- nodes ]))
  where nodes = filter (\(n,flow,_) -> flow /= 0 || n == "AA" ) ig

acost :: Graph -> Node -> Node -> Int
acost (_,arcs) from to = (arcs !! from) !! to

nflow :: Graph -> Node -> Int
nflow (flows,_) node = flows !! node

parseValve :: String -> InputNode
parseValve s = (name,flow,tunnels)
  where w = words s
        name = w !! 1
        flow = read (drop 5 (init (w !! 4)))
        tunnels = (map init (init (drop 9 w))) ++ [last w]

type State = ([Node],Node,Node,Int,Int)
type Queue = PQ.MaxPQueue Int State

search :: Graph -> (Int,Queue) -> (Int,Queue)
search g (best,q) = (newmax, (foldr (\(f,s) -> PQ.insert f s) nq [ (f,s) | (f,s) <- f1++f2, heuristic g (f,s) > newmax ]))
  where ((flow,(nodes,n1,n2,t1,t2)), nq) = PQ.deleteFindMax q
        f1 = [ ((flow + (nflow g next) * tleft), ((filter (\n -> n /= next) nodes),next,n2,tleft,t2)) | next <- nodes, let cost = acost g n1 next, let tleft = t1 - cost, tleft > 0 ]
        f2 = [ ((flow + (nflow g next) * tleft), ((filter (\n -> n /= next) nodes),n1,next,t1,tleft)) | next <- nodes, let cost = acost g n2 next, let tleft = t2 - cost, tleft > 0 ]
        newmax = maximum (best:(map fst (f1++f2)))

heuristic :: Graph -> (Int,State) -> Int
heuristic g (flow,(nodes,n1,n2,t1,t2)) = flow + (sum [ maximum [0,((t1 - (acost g n1 node)) * (nflow g node)),((t2 - (acost g n2 node)) * (nflow g node))] | node <- nodes ])

aoc :: [String] -> String
aoc f = show (fst (last (takeWhile (\(best,queue) -> not (PQ.null queue)) (iterate (search g) (0,PQ.singleton 0 ([ node | node <- [0..num-1], node /= start ],start,start,26,26))))))
  where (start,num,g) = reduce (map parseValve f)
