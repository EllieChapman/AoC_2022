module Day12 where

import Data.Set
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Char
import Data.List

day12_part1 :: [String] -> Int
day12_part1 xs = do
    let l = parse xs 1

    let height_map = make_map l
    let just_reached = Data.Set.fromList (make_just_reached l)
    let unreached = Data.Set.fromList (make_unreached l)
    let previously_reached :: Set (Int, Int) = Data.Set.fromList []
    find_path just_reached previously_reached unreached height_map 1


day12_part2 :: [String] -> Int
day12_part2 xs = do
    let l = parse xs 1

    let height_map = make_map l
    let just_reached = Data.Set.fromList (make_just_reached2 l)
    let unreached = Data.Set.fromList (make_unreached2 l)
    let previously_reached :: Set (Int, Int) = Data.Set.fromList []
    find_path just_reached previously_reached unreached height_map 1


find_path :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> M.Map (Int, Int) Char -> Int -> Int
find_path just_reached previously_reached unreached height_map steps_so_far = do
    let new_set = now_reachable just_reached height_map
    if found_end new_set height_map
    then steps_so_far
    else do
        let new_just_reached = Data.Set.difference (Data.Set.difference new_set previously_reached) just_reached
        find_path new_just_reached (Data.Set.union just_reached previously_reached) (Data.Set.difference unreached new_just_reached) height_map (steps_so_far + 1)


found_end :: Set (Int, Int) -> M.Map (Int, Int) Char -> Bool
found_end xs m = do
    let ls = Data.Set.toList xs
    case ls of
        l:ls -> do
            let v = fromMaybe '?' (m !? l)
            if v == 'E'
            then True
            else found_end (Data.Set.fromList ls) m
        [] -> False

now_reachable :: Set (Int, Int) -> M.Map (Int, Int) Char -> Set (Int, Int)
now_reachable xs m = do
    let ls = Data.Set.toList xs
    Data.Set.fromList (concat (Data.List.map (reachable_from_point m) ls))


reachable_from_point :: M.Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
reachable_from_point m (r, c) = do
    let current_height = fromMaybe '?' (m !? (r, c))
    Prelude.filter (\x -> (test m x current_height)) [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]


test :: M.Map (Int, Int) Char -> (Int, Int) -> Char -> Bool
test m (a, b) current_height = do
    let height = fromMaybe '~' (m !? (a, b))
    if height == 'E'
    then ord 'z' <= ((ord current_height)+1)
    else ord height <= ((ord current_height)+1)


make_unreached :: [(Int, Int, Char)] -> [(Int, Int)]
make_unreached xs =
    case xs of
        x:xs -> do
            let (a,b,c) = x
            if c == 'S'
            then (make_unreached xs)
            else (a,b):(make_unreached xs)
        [] -> []

make_unreached2 :: [(Int, Int, Char)] -> [(Int, Int)]
make_unreached2 xs =
    case xs of
        x:xs -> do
            let (a,b,c) = x
            if c == 'S' || c == 'a'
            then (make_unreached2 xs)
            else (a,b):(make_unreached2 xs)
        [] -> []

make_just_reached :: [(Int, Int, Char)] -> [(Int, Int)]
make_just_reached xs =
    case xs of
        x:xs -> do
            let (a,b,c) = x
            if c == 'S'
            then [(a,b)]
            else make_just_reached xs
        [] -> error "failed to find start"

make_just_reached2 :: [(Int, Int, Char)] -> [(Int, Int)]
make_just_reached2 xs =
    case xs of
        x:xs -> do
            let (a,b,c) = x
            if c == 'S' || c == 'a'
            then (a,b):(make_just_reached2 xs)
            else make_just_reached2 xs
        [] -> []

make_map :: [(Int, Int, Char)] -> M.Map (Int, Int) Char
make_map xs =
    case xs of
        x:xs -> do
            let (a, b, c) = x
            if c == 'S'
            then
                M.insert (a, b) 'a' (make_map xs)
            else
                M.insert (a, b) c (make_map xs)
        [] -> M.fromList []


parse :: [String] -> Int -> [(Int, Int, Char)]
parse xs n =
    case xs of
        x:xs ->
            (parse_line x n 1) ++ (parse xs (n+1))
        [] -> []

-- returns (x coor, y coord, value)
parse_line :: String -> Int -> Int -> [(Int, Int, Char)]
parse_line xs row column =
    case xs of
        x:xs -> (column, row, x):(parse_line xs row (column+1))
        [] -> []