module Top where

import Day1

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile p =  do
    s <- readFile p
    pure (lines s)



main :: IO ()
main = do

    putStrLn "*AoC2022*"

    putStrLn "*Day 1 Part 1*"
    _xs <- readLinesFromFile "src/day1_input.txt"
    print (day1_part1 _xs)
    putStrLn "*Day 1 Part 2*"
    print (day1_part2 _xs)


