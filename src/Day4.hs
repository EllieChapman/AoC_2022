module Day4 where

import Day1

day4_part1 :: [String] -> Int
day4_part1 xs =
    case xs of
        x:xs -> subset x + day4_part1 xs
        [] -> 0


subset:: String -> Int
subset x =
    case parse_four x of
        a:b:c:d:_xs -> contained a b c d
        _ -> error "dont have exactly 4 elements"

contained :: Int -> Int -> Int -> Int -> Int
contained a b c d =
    if
        | a == c -> 1
        | b == d -> 1
        | a < c && b > d -> 1
        | c < a && d > b -> 1
        | otherwise -> 0

parse_four :: String -> [Int]
parse_four x = do
    let xs = splitOn ',' x
    map read (concat (map (splitOn '-') xs))



day4_part2 :: [String] -> Int
day4_part2 xs =
    case xs of
        x:xs -> subset2 x + day4_part2 xs
        [] -> 0


subset2:: String -> Int
subset2 x =
    case parse_four x of
        a:b:c:d:_xs -> contained2 a b c d
        _ -> error "dont have exactly 4 elements"

contained2 :: Int -> Int -> Int -> Int -> Int
contained2 a b c d =
    if
        | b == c -> 1
        | a == d -> 1
        | b == d -> 1
        | a == c -> 1
        | c < b && c > a -> 1
        | d < b && d > a -> 1
        | a < d && a > c -> 1
        | b < d && b > c -> 1
        | otherwise -> 0
