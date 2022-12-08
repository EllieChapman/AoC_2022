module Day8 where

import Data.List
import Data.Set
import Data.Map.Strict as M
import Data.Map

day8_part1 :: [String] -> Int
day8_part1 xs = do
    let dir1 = make_tuples xs 1
    let dir2 = transpose dir1
    length (Data.Set.fromList (find_visible dir1 ++ find_visible dir2))

day8_part2 :: [String] -> Int
day8_part2 xs = do
    let dir1 = make_tuples xs 1
    let dir2 = transpose dir1
    -- length (fromList (find_visible dir1 ++ find_visible dir2))
    let m1 = score dir1 (Data.Map.fromList [])
    let m2 = score dir2 (Data.Map.fromList [])
    let m = M.unionWith (*) m1 m2
    head (reverse (sort (M.elems m)))
    -- M.elems m

    -- need to merge the two maps, making sure to mutiply scores for same keys, then find and return biggest value


score :: [[(Int, Int)]] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
score xss m =
    case xss of
        xs:xss -> do
            let new_m = row_score xs m
            score xss new_m
        [] -> m


row_score :: [(Int, Int)] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
row_score xs m = do
    let new_m = single_direction_row_score xs m
    single_direction_row_score (reverse xs) new_m


single_direction_row_score :: [(Int, Int)] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
single_direction_row_score xs m = do
    case xs of
        a:as -> do
            let s = find_score xs
            let new_m = M.insertWith (*) a s m
            -- let new_m = M.insertWith (\new old -> old * new) a s m
            single_direction_row_score as new_m
        [] -> m
    

-- get score for first tree in list
find_score :: [(Int, Int)] -> Int
find_score xs =
    case xs of
        x:xs -> do
            let (height, _) = x
            score_single xs height 1
        [] -> 0

score_single :: [(Int, Int)] -> Int -> Int -> Int
score_single xs height n =
    case xs of
        x:xs -> do
            let (a, _) = x
            if a < height
            then score_single xs height (n+1)
            else n
        [] -> n-1



find_visible :: [[(Int, Int)]] -> [(Int, Int)]
find_visible xss =
    case xss of
        xs:xss -> (find_visible_in_row xs) ++ (find_visible xss)
        [] -> []

find_visible_in_row :: [(Int, Int)] -> [(Int, Int)]
find_visible_in_row xs =
    one_view xs (-1) ++ one_view (reverse xs) (-1)


one_view :: [(Int, Int)] -> Int -> [(Int, Int)]
one_view xs max_seen =
    case xs of
        x:xs -> do
            let (a, _) = x
            if a > max_seen
            then x:one_view xs a
            else one_view xs max_seen
        [] -> []


make_tuples :: [String] -> Int -> [[(Int, Int)]]
make_tuples xs n =
    case xs of
        x:xs -> do
            let l = length x
            [make_tuples_row x n] ++ make_tuples xs (n+l)
        [] -> []

make_tuples_row :: String -> Int -> [(Int, Int)]
make_tuples_row xs n =
    case xs of
        x:xs -> (read [x], n):(make_tuples_row xs (n+1))
        [] -> []