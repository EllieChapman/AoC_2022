module Day3 where

import Data.List
import Data.Char

day3_part1 :: [String] -> Int
day3_part1 xs =
    case xs of
        x:xs -> score x + day3_part1 xs
        [] -> 0


score :: String -> Int
score xs = do
    let (as, bs) = halve_sort_list xs
    let c = find_same as bs
    value c


halve_sort_list :: String -> (String, String)
halve_sort_list xs = do
    let l = length xs `div` 2
    (sort(take l xs), sort(drop l xs))

value :: Char -> Int
value c =
    if isLower c
    then ord c - ord 'a' + 1
    else ord c - ord 'A' + 27

find_same :: String -> String -> Char
find_same as bs = do
    let a = head as
    let b = head bs
    if
        | a < b -> find_same (tail as) bs
        | a > b -> find_same as (tail bs)
        | otherwise -> a


day3_part2 :: [String] -> Int
day3_part2 xs =
    case xs of
        x:y:z:xs -> score2 x y z + day3_part2 xs
        [] -> 0
        _ -> error "not multiple of three"


score2 :: String -> String -> String -> Int
score2 xs ys zs = do
    let c = find_same2 (sort xs) (sort ys) (sort zs)
    value c

find_same2 :: String -> String -> String -> Char
find_same2 as bs cs = do
    let xs = find_sames as bs
    case find_sames xs cs of
        x:_ -> x
        a -> error ("not exactly one thing " ++ show xs ++ ", " ++ show cs ++ ", " ++ show a)


find_sames :: String -> String -> String
find_sames as bs =
    case as of
        a:as' ->
            case bs of
                b:bs' ->
                    if
                        | a < b -> find_sames as' bs
                        | a > b -> find_sames as bs'
                        | otherwise -> a:find_sames as' bs'
                [] -> []
        [] -> []