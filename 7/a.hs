module Aoc where

import Data.List

import Debug.Trace

data Tree = Dir String [Tree] | File String Int deriving Show

type State = (Tree, String)

parent :: String -> String
parent "" = ""
parent dir
  | last dir == '/' = dir
  | otherwise = parent (init dir)

parseCmd :: State -> String -> [String] -> State
parseCmd state "$ ls" s = parse state s
parseCmd (tree,cwd) "$ cd /" s = parse (tree,"/") s
parseCmd (tree,cwd) "$ cd .." s = parse (tree,parent (init cwd)) s
parseCmd (tree,cwd) cmd s = parse (tree,cwd ++ (drop 5 cmd) ++ "/") s -- "$ cd "

addDir :: Tree -> String -> Tree
addDir (Dir name subtree) path
  | isPrefixOf (name ++ "/") path && elem '/' (drop ((length name)+1) path) = Dir name [ addDir s (drop ((length name)+1) path) | s <- subtree ]
  | isPrefixOf (name ++ "/") path = Dir name ((Dir (drop ((length name)+1) path) []):subtree)
  | otherwise = Dir name subtree
addDir file _ = file

parseDir :: State -> String -> [String] -> State
parseDir (tree,cwd) dir s = parse (addDir tree (cwd ++ (drop 4 dir)),cwd) s

addFile :: Tree -> String -> Int -> Tree
addFile (Dir name subtree) path size
  | isPrefixOf (name ++ "/") path && elem '/' (drop ((length name)+1) path) = Dir name [ addFile s (drop ((length name)+1) path) size | s <- subtree ]
  | isPrefixOf (name ++ "/") path = Dir name ((File (drop ((length name)+1) path) size):subtree)
  | otherwise = Dir name subtree
addFile file _ _ = file

parseFile :: State -> (Int, String) -> [String] -> State
parseFile (tree,cwd) (size,' ':name) s = parse (addFile tree (cwd ++ name) size,cwd) s

parse :: State -> [String] -> State
parse state (cmd:s)
  | cmd !! 0 == '$' = parseCmd state cmd s
  | (take 3 cmd) == "dir" = parseDir state cmd s
  | otherwise = parseFile state (head (reads cmd)) s
parse state [] = state

totalSize :: Tree -> Int
totalSize (Dir name subtree) = sum (map totalSize subtree)
totalSize (File name size) = size

dirSizes :: Tree -> [(String,Int)]
dirSizes (Dir name subtree) = foldr (++) [(name,totalSize (Dir name subtree))] [dirSizes s | s <- subtree]
dirSizes (File _ _) = []

aoc :: [String] -> String
aoc f = show (sum [ size | (name,size) <- dirSizes (fst (parse ((Dir "" []), "/") f)), size <= 100000 ])
