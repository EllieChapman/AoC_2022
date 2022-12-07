module Day6 where

import Data.Set

day6_part1 :: [String] -> Int
day6_part1 xs =
    get_pos (head xs) 4 4


day6_part2 :: [String] -> Int
day6_part2 xs =
    get_pos (head xs) 14 14


get_pos :: String -> Int -> Int -> Int
get_pos s pos take_n = do
    let x = Prelude.take take_n s
    if check_unique x take_n
    then pos
    else get_pos (tail s) (pos+1) take_n


check_unique :: String -> Int -> Bool
check_unique s n =
    if length (fromList s) == n
    then True
    else False