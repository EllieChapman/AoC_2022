module Day7 where

import Day1
import Data.List


day7_part1 :: [String] -> Int
day7_part1 xs = do
    let (t, _is) = (parse_level (tail xs) [] [])
    sum (filter (\n -> n<100000) (map size (subtrees t)))

day7_part2 :: [String] -> Int
day7_part2 xs = do
    let (t, _is) = (parse_level (tail xs) [] [])
    let min_size = 30000000 - (70000000 - size t)
    head (sort (filter (\n -> n>min_size) (map size (subtrees t))))


parse_level :: [String] -> [Int] -> [Tree] -> (Tree, [String])
parse_level lines parsed_ints parsed_trees = do
    case lines of
        x:xs -> do
            let a:b:cs = splitOn ' ' x
            if
                | (a == "dir") || (b == "ls") -> parse_level xs parsed_ints parsed_trees
                | (b == "cd") ->
                    case cs of
                        c:_ ->
                            if c == ".."
                            then (Level parsed_ints parsed_trees, xs)
                            else do
                                let (t, is) = parse_level xs [] []
                                parse_level is parsed_ints (parsed_trees ++ [t])
                        _ -> error "should always have third element to parse"
                | otherwise -> parse_level xs (parsed_ints ++ [read a]) parsed_trees
            -- if "ls" or dir ignore, recursive call passing through same plus instuctions minus this line
            -- if int line, add to ints, recursive call same minus this line
            -- if cd name line, recusrive call, add to parsed trees, recrusive call again with returned remaining instrcutions
            -- if cd .., make tree out of parsed ints and parsed trees and return, plus return remaining instructions in tuple
        [] ->
            (Level parsed_ints parsed_trees, [])
            -- no more instrutions, so make tree out of parsed ints and trees, return as tuple with empty string


data Tree = Level [Int] [Tree] deriving (Show)


subtrees :: Tree -> [Tree]
subtrees t =
    case t of
        Level _is ts ->
            [t] ++ concat (map subtrees ts)


size :: Tree -> Int
size = sum . flatten


flatten :: Tree -> [Int]
flatten t = do
    case t of
        Level is ts ->
            is ++ concat (map flatten ts)