module Aoc where

import Data.List
import Data.List.Split
import Data.Either
import Text.Parsec
import Text.Parsec.String

data Item = List [Item] | Number Int deriving Show

instance Eq (Item) where
  a == b = 
    let (stop,result) = compareItems (a,b)
    in not stop

instance Ord (Item) where
  compare a b = 
    let (stop,result) = compareItems (a,b)
    in if stop then (if result then LT else GT) else EQ

int :: Parser Int
int = read <$> (many1 digit)

number :: Parser Item
number = Number <$> int

list :: Parser Item
list = List <$> (between (char '[') (char ']') (Text.Parsec.sepBy item (char ',')))

item :: Parser Item
item = list <|> number

parseItem :: String -> Item
parseItem s = 
  let Right result = parse (item <* eof) "" s
  in result

compareItems :: (Item,Item) -> (Bool,Bool)

compareItems (Number left, Number right)
  | left < right = (True,True)
  | left > right = (True,False)
  | left == right = (False,False)

compareItems (List left, Number right) = compareItems (List left, List [Number right])
compareItems (Number left, List right) = compareItems (List [Number left], List right)

compareItems (List [], List []) = (False,False)
compareItems (List left, List []) = (True,False)
compareItems (List [], List right) = (True,True)

compareItems (List (left:ls), List (right:rs))
  | stop = (stop, result)
  | otherwise = compareItems (List ls, List rs)
  where (stop,result) = compareItems (left, right)

divider1 = List [List [Number 2]]
divider2 = List [List [Number 6]]

aoc :: [String] -> String
aoc f = show (product [ index | (index,item) <- zip [1..] (sort ((map parseItem (filter (\x -> x /= "") f))++[divider1,divider2])), item == divider1 || item == divider2 ])
