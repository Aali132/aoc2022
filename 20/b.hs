module Aoc where

import Data.List

type Seq = [(Int,Int)]

cmod :: Integral a => a -> a -> a
cmod a b
  | m < 0 = m + b
  | otherwise = m
  where m = mod a b

offsetPos :: Int -> Int -> Int -> Int -> Int
offsetPos m num pos npos
  | pos == npos = end
  | opos > 0 && opos <= end = opos - 1
  | otherwise = opos
  where start = 0
        end = applyKeyMod num (m-1)
        o = cmod (start - pos) m
        opos = cmod (npos + o) m

offsetNeg :: Int -> Int -> Int -> Int -> Int
offsetNeg m num pos npos
  | pos == npos = end
  | opos >= end && opos < start = opos + 1
  | otherwise = opos
  where start = applyKeyMod (-num) (m-1)
        end = 0
        o = cmod (start - pos) m
        opos = cmod (npos + o) m

offset :: Int -> Int -> Int -> Int -> Int
offset m num pos npos
  | num == 0 = npos
  | num > 0 = offsetPos m num pos npos
  | num < 0 = offsetNeg m num pos npos

mix :: Seq -> Int -> Seq
mix seq index = [ (n,offset (length seq) num pos npos) | (n,npos) <- seq ]
  where (num,pos) = seq !! index

parse :: [String] -> Seq
parse s = zip (map read s) [0..]

index :: Seq -> Int -> Int
index seq i = fst x
  where Just x = find (\(n,p) -> p == i) seq

unmix seq = map fst (sortBy (\(a,b) (c,d) -> compare b d ) seq)

applyKeyMod :: Int -> Int -> Int
applyKeyMod n m = fromInteger (cmod ((toInteger n) * 811589153) (toInteger m))

applyKey :: Int -> Integer
applyKey n = (toInteger n) * 811589153

mixRound :: Seq -> Seq
mixRound seq = foldl mix seq [0..(length seq)-1]

aoc :: [String] -> String
aoc f = show (sum (map applyKey (map (index m) ind)))
  where seq = parse f
        miter = iterate mixRound seq
        m = miter !! 10
        ind = map (\x -> cmod (x+zind) (length seq)) [1000,2000,3000]
        Just (_,zind) = find (\(n,p) -> n == 0) m
