module Day10 where

import qualified Day1
import qualified Data.Set

day10_part1 :: [String] -> Int
day10_part1 xs =
    sum (clock (parse xs) 0 1 [])


day10_part2 :: [String] -> String
day10_part2 xs = do
    let sprite_pos = clock2 (parse xs) 0 1 [[0,1,2]]
    let ls = upto 0 ((length sprite_pos)-1)
    -- (length sprite_pos, length ls)
    split_lines (find_lit sprite_pos ls) 1

split_lines :: String -> Int -> String
split_lines xs n =
    case xs of
        x:xs ->
            if n /= 40
            then x:(split_lines xs (n+1))
            else x:'\n':(split_lines xs 1)
        [] -> []



find_lit :: [[Int]] -> [Int] -> String
find_lit xs is =
    case xs of
        x:xs ->
            [(lit x ((head is) `mod` 40))] ++ (find_lit xs (tail is))
        [] -> []


lit :: [Int] -> Int -> Char
lit xs i =
    if length (Data.Set.fromList (i:xs)) == 4
    then '.'
    else '#'



parse :: [String] -> [String]
parse xs =
    case xs of
        x:xs ->
            if take 4 x == "addx"
            then "noop":x:parse xs
            else x:parse xs
        [] -> []


upto :: Int -> Int -> [Int]
upto a b =
  if a < (b+1)
  then a : upto (a+1) b
  else []


clock :: [String] -> Int -> Int -> [Int] -> [Int]
clock is t x results =
    case is of
        i:is ->
            if t `mod` 40 == 19
            then
                clock is (t+1) (tick i x) ((x* (t+1)):results)
            else
                clock is (t+1) (tick i x) results
        [] ->
            if t `mod` 40 == 19
            then
                (x* (t+1)):results
            else
                results


clock2 :: [String] -> Int -> Int -> [[Int]] -> [[Int]]
clock2 is t x results =
    case is of
        i:is -> do
            let new_x = tick i x
            clock2 is (t+1) new_x (results ++ [[new_x - 1, new_x, new_x + 1]])
        [] ->
            results


tick :: String -> Int -> Int
tick s x =
    if take 4 s == "noop"
    then x
    else do
        let as = Day1.splitOn ' ' s
        x + (read (head (tail as)))
