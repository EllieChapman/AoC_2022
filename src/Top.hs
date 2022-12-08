module Top where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile p =  do
    s <- readFile p
    pure (lines s)


check :: (Eq a, Show a) => a -> a -> a
check a b = if a == b then a else error ("check failed: " ++ show a ++ " not same as: " ++ show b)


main :: IO ()
main = do

    putStrLn "*AoC2022*"

    putStrLn "*Day 1 Part 1*"
    _xs <- readLinesFromFile "src/day1_input.txt"
    print (check 71471 (day1_part1 _xs))
    putStrLn "*Day 1 Part 2*"
    print (check 211189 (day1_part2 _xs))

    putStrLn "*Day 2 Part 1*"
    _bs <- readLinesFromFile "src/day2_input.txt"
    print (check 13809 (day2_part1 _bs))
    putStrLn "*Day 2 Part 2*"
    print (check 12316 (day2_part2 _bs))

    putStrLn "*Day 3 Part 1*"
    _bs <- readLinesFromFile "src/day3_input.txt"
    -- print (check 157 (day3_part1 _bs))
    print (check 7863 (day3_part1 _bs))
    putStrLn "*Day 3 Part 2*"
    print (check 2488 (day3_part2 _bs))

    putStrLn "*Day 4 Part 1*"
    _bs <- readLinesFromFile "src/day4_input.txt"
    print (check 536 (day4_part1 _bs))
    putStrLn "*Day 4 Part 2*"
    print (check 845 (day4_part2 _bs))

    putStrLn "*Day 5 Part 1*"
    _bs <- readLinesFromFile "src/day5_input.txt"
    -- print (check ("CMZ") (day5_part1 _bs))
    print (check ("SBPQRSCDF") (day5_part1 _bs))
    putStrLn "*Day 5 Part 2*"
    print (check ("RGLVRCQSB") (day5_part2 _bs))

    putStrLn "*Day 6 Part 1*"
    _bs <- readLinesFromFile "src/day6_input.txt"
    print (check 1134 (day6_part1 _bs))
    putStrLn "*Day 6 Part 2*"
    print (check 2263 (day6_part2 _bs))

    putStrLn "*Day 7 Part 1*"
    _bs <- readLinesFromFile "src/day7_input.txt"
    -- print (check 95437 (day7_part1 _bs))
    print (check 1334506 (day7_part1 _bs))
    -- Day7.main
    putStrLn "*Day 7 Part 2*"
    print (check 7421137 (day7_part2 _bs))

    putStrLn "*Day 8 Part 1*"
    _bs <- readLinesFromFile "src/day8_input.txt"
    -- print (check 21 (day8_part1 _bs))
    print (check 1672 (day8_part1 _bs))
    putStrLn "*Day 8 Part 2*"
    print (check 327180 (day8_part2 _bs))
