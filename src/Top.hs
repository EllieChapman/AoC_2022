module Top where

import Day1
import Day2

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


