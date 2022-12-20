module Aoc where

import Data.Maybe

data Robot = Ore | Clay | Obsidian | Geode

type Res = (Int,Int,Int,Int)

type State = (Int,Res,Res)

type Blueprint = (Int,Int,Int,Int,Int,Int)

start :: State
start = (30,(1,0,0,0),(2,0,0,0))

canBuild :: Blueprint -> State -> [Robot]
canBuild (ore0,ore1,ore2,clay2,ore3,obs3) (_,_,(ore,clay,obs,_)) = catMaybes [ rOre,rClay,rObs,rGeode ]
  where rOre = if ore >= ore0 then Just Ore else Nothing
        rClay = if ore >= ore1 then Just Clay else Nothing
        rObs = if ore >= ore2 && clay >= clay2 then Just Obsidian else Nothing
        rGeode = if ore >= ore3 && obs >= obs3 then Just Geode else Nothing

newStates :: Blueprint -> State -> [State]
newStates _ (0,_,_) = []
newStates bp s = (prod:[ build bp prod r | r <- cb ])
  where (t,(rob0,rob1,rob2,rob3),(res0,res1,res2,res3)) = s
        cb = canBuild bp s
        prod = (t-1,(rob0,rob1,rob2,rob3),(res0+rob0,res1+rob1,res2+rob2,res3+rob3))

build :: Blueprint -> State -> Robot -> State
build (ore0,ore1,ore2,clay2,ore3,obs3) (t,(rob0,rob1,rob2,rob3),(res0,res1,res2,res3)) Ore = (t,(rob0+1,rob1,rob2,rob3),(res0-ore0,res1,res2,res3))
build (ore0,ore1,ore2,clay2,ore3,obs3) (t,(rob0,rob1,rob2,rob3),(res0,res1,res2,res3)) Clay = (t,(rob0,rob1+1,rob2,rob3),(res0-ore1,res1,res2,res3))
build (ore0,ore1,ore2,clay2,ore3,obs3) (t,(rob0,rob1,rob2,rob3),(res0,res1,res2,res3)) Obsidian = (t,(rob0,rob1,rob2+1,rob3),(res0-ore2,res1-clay2,res2,res3))
build (ore0,ore1,ore2,clay2,ore3,obs3) (t,(rob0,rob1,rob2,rob3),(res0,res1,res2,res3)) Geode = (t,(rob0,rob1,rob2,rob3+1),(res0-ore3,res1,res2-obs3,res3))

expandStates :: Blueprint -> [State] -> [State]
expandStates bp ss = pruneObs (pruneGeode (concat (map (newStates bp) ss)))

countGeodeRobs :: State -> Int
countGeodeRobs (_,(_,_,_,rob3),_) = rob3

pruneGeode :: [State] -> [State]
pruneGeode ss = filter (\(_,(_,_,_,rob3),_) -> rob3 >= (gc - 1)) ss
  where gc = maximum (map countGeodeRobs ss)

countObsRobs :: State -> Int
countObsRobs (_,(_,_,rob2,_),_) = rob2

pruneObs :: [State] -> [State]
pruneObs ss = filter (\(_,(_,_,rob2,_),_) -> rob2 >= (oc - 1)) ss
  where oc = maximum (map countObsRobs ss)

countGeodes :: State -> Int
countGeodes (_,_,(_,_,_,res3)) = res3

evalBlueprint :: Blueprint -> Int
evalBlueprint bp = maximum (map countGeodes ((iterate (expandStates bp) [start]) !! 30))

parse :: String -> Blueprint
parse s = (read (ws !! 6),read (ws !! 12),read (ws !! 18),read (ws !! 21),read (ws !! 27),read (ws !! 30))
  where ws = words s

aoc :: [String] -> String
aoc f = show(product bps)
  where bps = map evalBlueprint (take 3 (map parse f))
