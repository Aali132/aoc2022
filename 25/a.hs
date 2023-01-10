module Aoc where

import Data.Digits

dig '2' = 4
dig '1' = 3
dig '0' = 2
dig '-' = 1
dig '=' = 0

undig 4 = '2'
undig 3 = '1'
undig 2 = '0'
undig 1 = '-'
undig 0 = '='

offset :: Int -> Int
offset l = div (5 ^ l) 2

snafuLen :: Int -> Int
snafuLen i = ceiling (logBase 5 (2 * (fromIntegral i)))

unsnafu :: String -> Int
unsnafu s = (unDigits 5 (map dig s)) - (offset (length s))

snafu :: Int -> String
snafu i = map undig (digits 5 (i + (offset (snafuLen i))))

aoc :: [String] -> String
aoc f = snafu (sum (map unsnafu f))
