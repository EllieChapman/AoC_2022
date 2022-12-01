module Day1 where

import Data.List

day1_part1 :: [String] -> Int
day1_part1 xs = do
    let xss = parse xs
    -- map :: (a -> b) -> [a] -> [b]
    let iss = map (map intify) xss
    let is = map sum_elf iss
    max_elf is

sum_elf :: [Int] -> Int
sum_elf is = sum is

max_elf :: [Int] -> Int
max_elf is = maximum is

parse :: [String] -> [[String]]
parse xs = do
    let (as, bs) = chop_one xs
    if bs == []
    then [as]
    else as:parse bs


chop_one :: [String] -> ([String], [String])
chop_one xs =
    case xs of 
        x:xs ->
            if x == ""
            then ([], xs)
            else let (as, bs) = chop_one xs in
                (x:as, bs)
                
        [] -> ([], [])

intify :: String -> Int
intify = read


day1_part2 :: [String] -> Int
day1_part2 xs = do
    let xss = parse xs
    -- map :: (a -> b) -> [a] -> [b]
    let iss = map (map intify) xss
    let is = map sum_elf iss
    let sorted = reverse(sort is)
    let top3 = take 3 sorted
    sum top3